%%% @author Christian Briones
%%% @doc Lazily-evaluated iterables for Erlang
%%% @end
-module(streams).
-compile([{inline, [{lazily, 2}]}]).

%% API exports
-export([
    yield/1,
    naturals/0,
    map/2,
    filter/2,
    take/2,
    to_list/1,
    to_map/1,
    iterate/2,
    flatmap/2,
    chain/2,
    uniq/1,
    foldl/3,
    zip/2,
    chunk/2
  ]).

-type stream(A) :: fun(() -> halt | {A, stream(A)}).

%% @doc
%% Attempts to retrieve the next element of `Stream'.
%%
%% Returns the next element along with the updated stream, or
%% `halt' if `Stream' is empty.
%% @end
-spec yield(Stream :: stream(A)) -> halt | {A, stream(A)}.
yield(F) when is_function(F) ->
  case F() of
    G when is_function(G) -> yield(G);
    Yield -> Yield
  end;
yield([X|Xs]) -> {X, Xs};
yield([]) -> halt;
yield(M) when is_map(M) ->
  yield(maps:to_list(M)).

lazily(F, Stream) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} -> F(X, Xs);
      halt -> halt
    end
  end.

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
    lazily(fun(X, Xs) ->
      Chained = chain(Fun(X), flatmap(Fun, Xs)),
      yield(Chained)
    end, Stream)
  end.

-spec uniq(stream(A)) -> stream(A).
uniq(Stream) ->
  do_uniq(Stream, #{}).

do_uniq(Stream, Seen) ->
  lazily(fun(X, Xs) ->
    case maps:is_key(X, Seen) of
      true ->
        do_uniq(Xs, Seen);
      false ->
        Rest = do_uniq(Xs, Seen#{X => true}),
        {X, Rest}
    end
  end, Stream).

%% @doc
%% Applies `Fun' to each element of `Stream', yielding a new stream of
%% the transformed elements.
%% @end
-spec map(fun((A) -> B), stream(A)) -> stream(B).
map(Fun, Stream) ->
  lazily(fun(X, Xs) ->
    {Fun(X), map(Fun, Xs)}
  end, Stream).

%% @doc
%% Returns a new stream consisting of all the elements in `Stream' for
%% which `Fun' returns true.
%% @end
-spec filter(fun((A) -> boolean()), stream(A)) -> stream(A)
  when A :: any().
filter(Fun, Stream) ->
  lazily(fun(X, Xs) ->
    case Fun(X) of
      true -> {X, filter(Fun, Xs)};
      false -> filter(Fun, Xs)
    end
  end, Stream).

%% @doc
%% Evaluates `Stream', collecting its elements into a list.
%%
%% Warning: If a stream is infinite this function this function will loop
%% indefinitely.
%% @end
-spec to_list(stream(A)) -> list(A).
to_list(Stream) ->
  Rev = streams:foldl(fun(A, Acc) ->
    [A|Acc]
  end, [], Stream),
  lists:reverse(Rev).

-spec to_map(stream({K, V})) -> map() when K :: any(), V :: any().
to_map(Stream) ->
  streams:foldl(fun({K, V}, Acc) ->
    Acc#{K => V}
  end, #{}, Stream).

-spec zip(stream(A), stream(B)) -> stream({A, B}).
zip(StreamA, StreamB) ->
  fun() ->
    case {yield(StreamA), yield(StreamB)} of
      {{A, As}, {B, Bs}} -> {{A, B}, zip(As, Bs)};
      {halt, _} -> halt;
      {_, halt} -> halt
    end
  end.

%% @doc
%% Returns a new stream consisting of the first `N' elements of `Stream'.
%% @end
-spec take(integer(), stream(A)) -> stream(A).
take(N, Stream) when N >= 0 ->
  lazily(fun(X, Xs) ->
    case N of
      0 -> halt;
      _ -> {X, take(N - 1, Xs)}
    end
  end, Stream).

-spec chunk(integer(), stream(A)) -> stream([A]).
chunk(N, Stream) ->
  fun() ->
    do_chunk(0, N, Stream, [])
  end.

do_chunk(N, N, Stream, Acc) ->
  {lists:reverse(Acc), chunk(N, Stream)};
do_chunk(M, N, Stream, Acc) ->
  case yield(Stream) of
    {X, Xs} ->
      do_chunk(M + 1, N, Xs, [X|Acc]);
    halt ->
      case Acc of
        [] -> halt;
        _ ->
          {lists:reverse(Acc), []}
      end
  end.

-spec foldl(fun((A, B) -> B), B, stream(A)) -> B.
foldl(F, Acc, Stream) ->
  foldl_priv(F, Acc, yield(Stream)).

foldl_priv(F, Acc, {X, Xs}) ->
  NewAcc = F(X, Acc),
  foldl_priv(F, NewAcc, yield(Xs));
foldl_priv(_F, Acc, halt) -> Acc.
