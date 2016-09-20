%%% @author Christian Briones
%%% @doc Lazily-evaluated iterables for Erlang
%%% @end
-module(streams).
-compile([{inline, [{lazily, 2}, {id, 1}]}]).

%% API exports
-export([
    yield/1,
    lazily/2,
    naturals/0,
    map/2,
    filter/2,
    filter_map/2,
    take/2,
    drop/2,
    to_list/1,
    to_map/1,
    iterate/2,
    flatmap/2,
    chain/2,
    uniq/1,
    uniq/2,
    foldl/3,
    zip/2,
    zip/3,
    chunk/2,
    take_while/2,
    drop_while/2,
    repeatedly/1,
    cycle/1,
    count/1,
    sum/1,
    unfold/2,
    with_index/1,
    with_index/2,
    transform/3,
    group_by/2,
    split/2,
    split_while/2
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

id(A) -> A.

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
flatmap(F, Stream) ->
  transform(fun(X, UnusedAcc) ->
    {F(X), UnusedAcc}
  end, undefined, Stream).

-spec uniq(stream(A)) -> stream(A).
uniq(Stream) ->
  uniq(fun id/1, Stream).

uniq(F, Stream) ->
  do_uniq(F, Stream, #{}).

do_uniq(F, Stream, Seen) ->
  lazily(fun(X, Xs) ->
    Key = F(X),
    case maps:is_key(Key, Seen) of
      true ->
        do_uniq(F, Xs, Seen);
      false ->
        Rest = do_uniq(F, Xs, Seen#{Key => true}),
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

-spec filter_map(fun((A) -> boolean()), stream(A)) -> stream(A)
  when A :: any().
filter_map(Fun, Stream) ->
  lazily(fun(X, Xs) ->
    case Fun(X) of
      {true, Y} -> {Y, filter_map(Fun, Xs)};
      false -> filter_map(Fun, Xs)
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
  zip(fun(A, B) -> {A, B} end, StreamA, StreamB).

-spec zip(fun((A, B) -> C), stream(A), stream(B)) -> stream(C).
zip(F, StreamA, StreamB) ->
  fun() ->
    case {yield(StreamA), yield(StreamB)} of
      {{A, As}, {B, Bs}} -> {F(A, B), zip(F, As, Bs)};
      {halt, _} -> halt;
      {_, halt} -> halt
    end
  end.

-spec with_index(stream(A)) -> stream({integer(), A}).
with_index(Stream) ->
  with_index(0, Stream).

-spec with_index(integer(), stream(A)) -> stream({integer(), A}).
with_index(Offset, Stream) ->
  lazily(fun(X, Xs) ->
    Y = {Offset, X},
    Ys = with_index(Offset + 1, Xs),
    {Y, Ys}
  end, Stream).

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

-spec take_while(fun((A) -> boolean()), stream(A)) -> stream(A).
take_while(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      true -> {X, take_while(F, Xs)};
      false -> halt
    end
  end, Stream).

-spec drop(non_neg_integer(), stream(A)) -> stream(A).
drop(0, Stream) -> Stream;
drop(N, Stream) ->
  lazily(fun(_, Xs) ->
    drop(N - 1, Xs)
  end, Stream).

-spec drop_while(fun((A) -> boolean()), stream(A)) -> stream(A).
drop_while(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      true -> drop_while(F, Xs);
      false -> {X, Xs}
    end
  end, Stream).

-spec repeatedly(fun(() -> A)) -> stream(A).
repeatedly(F) ->
  fun() ->
    {F(), repeatedly(F)}
  end.

-spec cycle(stream(A)) -> stream(A).
cycle(Stream) ->
  do_cycle(Stream, Stream).

do_cycle(Stream, Original) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} -> {X, do_cycle(Xs, Original)};
      halt -> do_cycle(Original, Original)
    end
  end.

-spec count(stream(any())) -> non_neg_integer().
count(Stream) ->
  foldl(fun(_, Acc) -> Acc + 1 end, 0, Stream).

-spec sum(stream(any())) -> number().
sum(Stream) ->
  foldl(fun(I, Acc) -> Acc + I end, 0, Stream).

-spec unfold(fun(({A, B}) -> {A, B}), A) -> stream(B).
unfold(F, Init) ->
  fun() ->
    {Init, do_unfold(F, Init)}
  end.

do_unfold(F, Acc) ->
  fun() ->
    case F(Acc) of
      halt -> halt;
      {X, Next} -> {X, do_unfold(F, Next)}
    end
  end.

group_by(F, Stream) ->
  group_by(F, Stream, undefined, []).

group_by(F, Stream, Key, Term) ->
  fun() ->
    case yield(Stream) of
      {X, Xs} ->
        case F(X) of
          Key -> group_by(F, Xs, Key, [X|Term]);
          NewKey ->
            case Term of
              [] -> group_by(F, Xs, NewKey, [X]);
              [_|_] -> {Term, group_by(F, Xs, NewKey, [X])}
            end
        end;
      halt ->
        case Term of
          [] -> halt;
          _ -> {lists:reverse(Term), []}
        end
    end
  end.

transform(F, Acc, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X, Acc) of
      halt -> halt;
      {[], Next} ->
        transform(F, Next, Xs);
      {[Y], Next} ->
        Ys = transform(F, Next, Xs),
        {Y, Ys};
      {Y, Next} ->
        Ys = transform(F, Next, Xs),
        chain(Y, Ys)
    end
  end, Stream).

split(N, Stream) ->
  split(N, Stream, []).

split(0, Stream, Acc) ->
  {lists:reverse(Acc), Stream};
split(N, Stream, Acc) ->
  case yield(Stream) of
    {X, Xs} -> split(N - 1, Xs, [X|Acc]);
    halt -> {lists:reverse(Acc), []}
  end.

split_while(F, Stream) ->
  split_while(F, Stream, []).

split_while(F, Stream, Acc) ->
  case yield(Stream) of
    {X, Xs} ->
      case F(X) of
        true -> split_while(F, Xs, [X|Acc]);
        false -> {lists:reverse(Acc), Stream}
      end;
    halt -> {lists:reverse(Acc), []}
  end.

-spec foldl(fun((A, B) -> B), B, stream(A)) -> B.
foldl(F, Acc, Stream) ->
  foldl_priv(F, Acc, yield(Stream)).

foldl_priv(F, Acc, {X, Xs}) ->
  NewAcc = F(X, Acc),
  foldl_priv(F, NewAcc, yield(Xs));
foldl_priv(_F, Acc, halt) -> Acc.
