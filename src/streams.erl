%%% @author Christian Briones
%%% @doc Lazily-evaluated iterables for Erlang
%%% @end
-module(streams).

%% API exports
-export([
    yield/1,
    naturals/0,
    map/2,
    filter/2,
    take/2,
    to_list/1,
    iterate/2,
    flatmap/2,
    chain/2,
    uniq/1,
    foldl/3
  ]).

-type stream(A) :: fun(() -> halt | {A, stream(A)}).

%% @doc
%% Attempts to retrieve the next element of `Stream'.
%%
%% Returns the next element along with the updated stream, or
%% `halt' if `Stream' is empty.
%% @end
-spec yield(Stream :: stream(A)) -> halt | {A, stream(A)}.
yield(F) when is_function(F) -> F();
yield([X|Xs]) -> {X, Xs};
yield([]) -> halt;
yield(M) when is_map(M) ->
  yield(maps:to_list(M)).

%% @doc
%% Returns the stream of all natural numbers.
%% @end
-spec naturals() -> stream(integer()).
naturals() -> iterate(fun(N) -> N + 1 end, 0).

-spec iterate(fun((A) -> A), A) -> stream(A).
iterate(F, Acc) ->
  fun() ->
    {Acc, do_iterate(F, Acc)}
  end.

do_iterate(F, Last) ->
  fun() ->
    Next = F(Last),
    {Next, do_iterate(F, Next)}
  end.

-spec chain(stream(A), stream(A)) -> stream(A) when A :: any().
chain(A, B) ->
  fun() ->
    case yield(A) of
      {X, Xs} -> {X, chain(Xs, B)};
      halt -> yield(B)
    end
  end.

-spec flatmap(fun((A) -> [B]), stream(A)) -> stream(B).
flatmap(Fun, Stream) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} ->
        Chained = chain(Fun(X), flatmap(Fun, Xs)),
        yield(Chained);
      halt -> halt
    end
  end.

-spec uniq(stream(A)) -> stream(A).
uniq(Stream) ->
  do_uniq(Stream, #{}).

do_uniq(Stream, Seen) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} ->
        case maps:is_key(X, Seen) of
          true ->
            yield(do_uniq(Xs, Seen));
          false ->
            Rest = do_uniq(Xs, Seen#{X => true}),
            {X, Rest}
        end;
      halt -> halt
    end
  end.

%% @doc
%% Applies `Fun' to each element of `Stream', yielding a new stream of
%% the transformed elements.
%% @end
-spec map(fun((A) -> B), stream(A)) -> stream(B).
map(Fun, Stream) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} -> {Fun(X), map(Fun, Xs)};
      halt -> halt
    end
  end.

%% @doc
%% Returns a new stream consisting of all the elements in `Stream' for
%% which `Fun' returns true.
%% @end
-spec filter(fun((A) -> boolean()), stream(A)) -> stream(A)
  when A :: any().
filter(Fun, Stream) ->
  fun() -> do_filter(Fun, Stream) end.

%% @private
do_filter(Fun, Stream) ->
  case yield(Stream) of
    {X, Xs} ->
      case Fun(X) of
        true -> {X, filter(Fun, Xs)};
        false -> do_filter(Fun, Xs)
      end;
    halt -> halt
  end.

%% @doc
%% Evaluates `Stream', collecting its elements into a list.
%%
%% Warning: If a stream is infinite this function this function will loop
%% indefinitely.
%% @end
-spec to_list(stream(A)) -> list(A).
to_list(Stream) -> to_list(Stream, []).

%% @private
to_list(Stream, Acc) ->
  case yield(Stream) of
    {X, Xs} -> to_list(Xs, [X|Acc]);
    halt -> lists:reverse(Acc)
  end.

%% @doc
%% Returns a new stream consisting of the first `N' elements of `Stream'.
%% @end
-spec take(integer(), stream(A)) -> stream(A).
take(N, Stream) when N >= 0 ->
  fun() ->
    case yield(Stream) of
      {X, Xs} ->
        case N of
          0 -> halt;
          _ -> {X, take(N - 1, Xs)}
        end;
      halt -> halt
    end
  end.

-spec foldl(fun((A, B) -> B), B, stream(A)) -> B.
foldl(F, Acc, Stream) ->
  foldl_priv(F, Acc, yield(Stream)).

foldl_priv(F, Acc, {X, Xs}) ->
  NewAcc = F(X, Acc),
  foldl_priv(F, NewAcc, yield(Xs));
foldl_priv(_F, Acc, halt) -> Acc.
