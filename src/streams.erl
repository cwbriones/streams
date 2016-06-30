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
    to_list/1
  ]).

-type stream(A) :: fun(() -> halt | {A, stream(A)}).

%% @doc
%% Attempts to retrieve the next element of `Stream'.
%%
%% Returns the next element along with the updated stream, or
%% `halt' if `Stream' is empty.
%% @end
-spec yield(Stream :: stream(A)) -> halt | {A, stream(A)}.
yield(F) when is_function(F) -> F().

%% @doc
%% Returns the stream of all natural numbers.
%% @end
-spec naturals() -> stream(integer()).
naturals() -> naturals(0).

%% @private
naturals(N) ->
  fun() ->
    {N, naturals(N + 1)}
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
