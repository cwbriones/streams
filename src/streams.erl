%%% @author Christian Briones
%%% @doc Lazily-evaluated iterables for Erlang
%%% @end
-module(streams).
-compile([{inline, [{lazily, 2}, {id, 1}]}]).

%% API exports

%% Stream transformers
-export([
    append/2,
    chunk/2,
    cycle/1,
    drop/2,
    drop_while/2,
    filter/2,
    filter_map/2,
    flat_map/2,
    group_by/2,
    lazily/2,
    map/2,
    split/2,
    split_while/2,
    take/2,
    take_while/2,
    transform/3,
    uniq/1,
    uniq/2,
    with_index/1,
    with_index/2,
    zip/2,
    zip/3
  ]).

%% Stream generators
-export([
    iterate/2,
    naturals/0,
    repeatedly/1,
    unfold/2
  ]).

%% Stream reducers
-export([
    count/1,
    fold/3,
    sum/1,
    to_list/1,
    to_map/1
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

%% @doc
%% Lazily applies a function to a stream. Somewhat equivalent to list comprehensions.
%%
%% Expects a function `F' which takes the head and tail of the given stream, returning
%% either `halt' or a new head/tail tuple pair.
%% @end
-spec lazily(fun(({A, stream(A)}) -> halt | {B, stream(B)}), stream(A)) -> stream(B).
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

%% @doc
%% Creates a stream by emitting `Acc' followed by the sequence
%% of values resulting from applying `F' to the previous value.
%% @end
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

%% @doc
%% Creates a stream that first yields all values from `StreamA' followed
%% by all values in `StreamB'.
%% @end
-spec append(stream(A), stream(A)) -> stream(A) when A :: any().
append(StreamA, StreamB) ->
  fun() ->
    case yield(StreamA) of
      {X, Xs} -> {X, append(Xs, StreamB)};
      halt -> yield(StreamB)
    end
  end.

%% @doc
%% Creates a stream by applying `F' to each element of `Stream',
%% concatenating the resulting streams.
%% @end
-spec flat_map(fun((A) -> stream(B)), stream(A)) -> stream(B).
flat_map(F, Stream) ->
  transform(fun(X, UnusedAcc) ->
    {F(X), UnusedAcc}
  end, undefined, Stream).

%% @doc
%% Creates a stream that yields every unique value of `Stream' exactly once.
%%
%% Note that this needs to keep a map to keep track of previously seen values and
%% can use quite a bit of memory for large streams.
%% @end
-spec uniq(stream(A)) -> stream(A).
uniq(Stream) ->
  uniq(fun id/1, Stream).

%% @doc
%% Creates a stream that yields every unique value of `Stream' exactly once according
%% to the key function `F'.
%%
%% Note that this needs to keep a map to keep track of previously seen values and
%% can use quite a bit of memory for large streams.
%% @end
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
%% Creates a stream consisting of all the elements in `Stream' for
%% which `F' returns `true'.
%% @end
-spec filter(fun((A) -> boolean()), stream(A)) -> stream(A)
  when A :: any().
filter(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      true -> {X, filter(F, Xs)};
      false -> filter(F, Xs)
    end
  end, Stream).

%% @doc
%% Maps and filters a stream in a single pass. The usage is identical
%% to `lists:filtermap/2'.
%% @end
-spec filter_map(fun((A) -> boolean()), stream(A)) -> stream(A)
  when A :: any().
filter_map(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      {true, Y} -> {Y, filter_map(F, Xs)};
      true -> {X, filter_map(F, Xs)};
      false -> filter_map(F, Xs)
    end
  end, Stream).

%% @doc
%% Evaluates `Stream', collecting its elements into a list.
%%
%% Warning: If `Stream' is infinite this function this function will loop
%% indefinitely.
%% @end
-spec to_list(stream(A)) -> list(A).
to_list(Stream) ->
  Rev = fold(fun(A, Acc) ->
    [A|Acc]
  end, [], Stream),
  lists:reverse(Rev).

%% @doc
%% Evaluates `Stream', collecting its elements into a map. The stream must
%% emit tuples.
%%
%% Warning: If `Stream' is infinite this function this function will loop
%% indefinitely.
%% @end
-spec to_map(stream({K, V})) -> map() when K :: any(), V :: any().
to_map(Stream) ->
  fold(fun({K, V}, Acc) ->
    Acc#{K => V}
  end, #{}, Stream).

%% @doc
%% Creates a stream by pairing up each element of `StreamA' and `StreamB'
%% into tuples.
%%
%% The resulting stream has the length of the shortest input stream.
%% @end
-spec zip(stream(A), stream(B)) -> stream({A, B}).
zip(StreamA, StreamB) ->
  zip(fun(A, B) -> {A, B} end, StreamA, StreamB).

%% @doc
%% Creates a stream by pairing up each element of `StreamA' and `StreamB'
%% through the function `F'.
%%
%% The resulting stream has the length of the shortest input stream.
%% @end
-spec zip(fun((A, B) -> C), stream(A), stream(B)) -> stream(C).
zip(F, StreamA, StreamB) ->
  fun() ->
    case {yield(StreamA), yield(StreamB)} of
      {{A, As}, {B, Bs}} -> {F(A, B), zip(F, As, Bs)};
      {halt, _} -> halt;
      {_, halt} -> halt
    end
  end.

%% @doc
%% Creates a stream of each element in `Stream' along with its 0-indexed
%% position.
%% @end
-spec with_index(stream(A)) -> stream({integer(), A}).
with_index(Stream) ->
  with_index(0, Stream).

%% @doc
%% Creates a stream of each element in `Stream' along with its 0-indexed
%% position, offset by `Offset'.
%% @end
-spec with_index(integer(), stream(A)) -> stream({integer(), A}).
with_index(Offset, Stream) ->
  lazily(fun(X, Xs) ->
    Y = {Offset, X},
    Ys = with_index(Offset + 1, Xs),
    {Y, Ys}
  end, Stream).

%% @doc
%% Creates a new stream consisting of the first `N' elements of `Stream'.
%% @end
-spec take(integer(), stream(A)) -> stream(A).
take(N, Stream) when N >= 0 ->
  lazily(fun(X, Xs) ->
    case N of
      0 -> halt;
      _ -> {X, take(N - 1, Xs)}
    end
  end, Stream).

%% @doc
%% Creates a stream that emits `N' element lists of contiguous elements
%% from `Stream'.
%% @end
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

%% @doc
%% Takes elements from `Stream' until `F' returns `false'.
%% @end
-spec take_while(fun((A) -> boolean()), stream(A)) -> stream(A).
take_while(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      true -> {X, take_while(F, Xs)};
      false -> halt
    end
  end, Stream).

%% @doc
%% Creates a stream that drops the first `N' elements of `Stream'.
%% @end
-spec drop(non_neg_integer(), stream(A)) -> stream(A).
drop(0, Stream) -> Stream;
drop(N, Stream) ->
  lazily(fun(_, Xs) ->
    drop(N - 1, Xs)
  end, Stream).

%% @doc
%% Drops elements from `Stream' until `F' returns `false'.
%% @end
-spec drop_while(fun((A) -> boolean()), stream(A)) -> stream(A).
drop_while(F, Stream) ->
  lazily(fun(X, Xs) ->
    case F(X) of
      true -> drop_while(F, Xs);
      false -> {X, Xs}
    end
  end, Stream).

%% @doc
%% Creates a stream of elements by repeatedly calling `F'.
%% @end
-spec repeatedly(fun(() -> A)) -> stream(A).
repeatedly(F) ->
  fun() ->
    {F(), repeatedly(F)}
  end.

%% @doc
%% Creates a stream by repeatedly going through `Stream', looping around when
%% it is exhausted.
%%
%% Note that if `Stream' is infinite this effectively returns the
%% same stream.
%%
%% Warning: If `Stream' is empty this function this function will loop
%% indefinitely.
%% @end
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

%% @doc
%% Counts the number of elements in `Stream'.
%%
%% Warning: If `Stream' is infinite this function this function will loop
%% indefinitely.
%% @end
-spec count(stream(any())) -> non_neg_integer().
count(Stream) ->
  fold(fun(_, Acc) -> Acc + 1 end, 0, Stream).

%% @doc
%% Returns the sum of elements in `Stream'.
%%
%% Warning: If `Stream' is infinite this function this function will loop
%% indefinitely.
%% @end
-spec sum(stream(any())) -> number().
sum(Stream) ->
  fold(fun(I, Acc) -> Acc + I end, 0, Stream).

%% @doc
%% Unfolds a stream from a seed value `Init'.
%%
%% The stream will yield `Init', and then call `F' with it as the initial
%% accumulator to generate subsequent values.
%%
%% `F' is expected to either return `{NextItem, NextAcc}' or `halt'.
%% @end
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

%% @doc
%% Creates a stream that groups continguous sequences of elements for which
%% `F' returns the same value.
%% @end
-spec group_by(fun((A) -> B), stream(A)) -> stream([A]) when B :: any().
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

%% @doc
%% Transforms an existing `Stream'.
%%
%% Transform expects a function that takes the next element of `Stream' and the current
%% accumulator, returning either `{Iterable, NewAccumulator}' or `halt', in which case
%% the resulting stream halts.
%% @end
-spec transform(fun((A, B) -> {stream(C), B} | halt), B, stream(A)) -> stream(C).
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
        append(Y, Ys)
    end
  end, Stream).

%% @doc
%% Splits off the first `N' elements of `Stream', returning these elements
%% in a list as well as the remaining stream.
%% @end
-spec split(non_neg_integer(), stream(A)) -> {[A], stream(A)}.
split(N, Stream) ->
  split(N, Stream, []).

split(0, Stream, Acc) ->
  {lists:reverse(Acc), Stream};
split(N, Stream, Acc) ->
  case yield(Stream) of
    {X, Xs} -> split(N - 1, Xs, [X|Acc]);
    halt -> {lists:reverse(Acc), []}
  end.

%% @doc
%% Collects elements from `Stream' into a list until `F' returns false.
%%
%% Returns the collected elements as well as the remaining stream.
%% @end
-spec split_while(fun((A) -> boolean()), stream(A)) -> {[A], stream(A)}.
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

%% @doc
%% Folds a stream into a single value by `F' to each element and the current
%% accumulator to compute the next accumulator.
%%
%% Returns the final accumulator.
%%
%% Warning: If `Stream' is infinite this function this function will loop
%% indefinitely.
%% @end
-spec fold(fun((A, B) -> B), B, stream(A)) -> B.
fold(F, Acc, Stream) ->
  fold_priv(F, Acc, yield(Stream)).

fold_priv(F, Acc, {X, Xs}) ->
  NewAcc = F(X, Acc),
  fold_priv(F, NewAcc, yield(Xs));
fold_priv(_F, Acc, halt) -> Acc.
