-module(streams_tests).
-include_lib("eunit/include/eunit.hrl").

list_test() ->
  List = lists:seq(1, 100),
  List = streams:to_list(List),
  ok.

iterate_test() ->
  Stream = streams:iterate(fun(I) ->
    I * 2
  end, 1),
  expect_elems([1, 2, 4, 8, 16], Stream),
  ok.

take_test() ->
  Stream = streams:take(6, streams:naturals()),
  [0, 1, 2, 3, 4, 5] = streams:to_list(Stream),
  ok.

map_test() ->
  FromList = streams:map(fun(A) ->
    A * 2
  end, [0, 1, 2, 3, 4, 5]),
  [0, 2, 4, 6, 8, 10] = streams:to_list(FromList),

  FromStream = streams:map(fun(A) ->
    A * 2
  end, streams:naturals()),
  expect_elems([0, 2, 4, 6, 8, 10], FromStream),
  ok.

filter_test() ->
  Odds = streams:filter(fun(A) ->
    A rem 2 == 1
  end, streams:naturals()),
  expect_elems([1, 3, 5, 7, 9, 11], Odds),
  ok.

flatmap_test() ->
  Stream = streams:flatmap(fun(A) ->
    [A, A * 2, A * 3]
  end, streams:naturals()),
  expect_elems([0, 0, 0, 1, 2, 3, 2, 4, 6], Stream),
  ok.

append_test() ->
  %% 0, 1, 2, 3, 4
  StreamA = streams:take(5, streams:naturals()),
  StreamB = [5, 6, 7, 8],
  StreamC = streams:append(StreamA, StreamB),
  [0, 1, 2, 3, 4, 5, 6, 7, 8] = streams:to_list(StreamC),
  ok.

uniq_test() ->
  UniqList = streams:uniq([1, 2, 3, 1, 2, 3, 1, 2, 3]),
  [1, 2, 3] = streams:to_list(UniqList),

  Duplicates = streams:flatmap(fun(A) -> [A, A, A] end, [1, 2, 3, 4, 5]),
  [1, 2, 3, 4, 5] = streams:to_list(streams:uniq(Duplicates)),
  ok.

uniq_by_test() ->
  Items = [
    #{a => 1, b => 1},
    #{a => 1, b => 2},
    #{a => 2, b => 3},
    #{a => 2, b => 4},
    #{a => 1, b => 5}
  ],
  Uniq = streams:uniq(fun(M) -> maps:get(a, M) end, Items),
  [
    #{a := 1, b := 1},
    #{a := 2, b := 3}
  ] = streams:to_list(Uniq),
  ok.

fold_test() ->
  6 = streams:fold(fun(A, Acc) -> A + Acc end, 0, [1, 2, 3]),

  Taken = streams:take(4, streams:naturals()),
  6 = streams:fold(fun(A, Acc) -> A + Acc end, 0, Taken),
  ok.

chunk_test() ->
  Chunks = streams:chunk(3, [1, 2, 3, 4, 5, 6, 7, 8]),
  [[1, 2, 3], [4, 5, 6], [7, 8]] = streams:to_list(Chunks),

  Single = streams:chunk(2, [1, 2]),
  [[1, 2]] = streams:to_list(Single),

  Empty = streams:chunk(2, []),
  [] = streams:to_list(Empty),
  ok.

take_while_test() ->
  Taken = streams:take_while(fun(I) ->
    I < 10
  end, streams:naturals()),
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] = streams:to_list(Taken),
  ok.

drop_test() ->
  Drop = streams:drop(10, streams:naturals()),
  expect_elems([10, 11, 12, 13, 14], Drop),
  ok.

drop_while_test() ->
  Drop = streams:drop_while(fun(I) ->
    I < 10
  end, streams:naturals()),
  expect_elems([10, 11, 12, 13, 14], Drop),
  ok.

cycle_test() ->
  Cycle = streams:cycle([1, 2, 3]),
  expect_elems([1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2], Cycle),
  ok.

sum_test() ->
  0 = streams:sum([]),
  15 = streams:sum([1, 2, 3, 4, 5]),
  6.0 = streams:sum([1.0, 2.0, 3.0]),
  ok.

count_test() ->
  0 = streams:count([]),
  Nat10 = streams:take(10, streams:naturals()),
  Nat100 = streams:take(100, streams:naturals()),
  10 = streams:count(Nat10),
  100 = streams:count(Nat100),
  ok.

filter_map_test() ->
  FilterMap = streams:filter_map(fun(I) ->
    case I rem 3 of
      0 -> false;
      1 -> true;
      2 -> {true, I * I}
    end
  end, streams:naturals()),
  expect_elems([1, 4, 4, 25, 7, 64, 10, 121, 13], FilterMap).

complicated_stream_test() ->
  %% 0, 1, 2, 3, 4..
  Naturals = streams:naturals(),
  %% 0, 1, 2, 2, 3, 3, 3, 4..
  FlatMap = streams:flatmap(fun(I) ->
    lists:seq(0, I)
  end, Naturals),
  %% 0, 1, 2, 3, 4..
  Uniq = streams:uniq(FlatMap),
  %% 1, 3, 5, 7..
  Filter = streams:filter(fun(A) -> A rem 2 == 1 end, Uniq),
  %% 2, 6, 10, 14
  Map = streams:map(fun(A) -> A * 2 end, Filter),
  %% Effectively unchanged
  Taken = streams:take(1000000, Map),
  %% {1, 2}, {2, 6}, {3, 10}, {4, 14}...
  Zipped = streams:zip([1, 2, 3, 4], Taken),
  %% [{1, 2}, {2, 6}, {3, 10}, {4, 14}] ...
  Chunk = streams:chunk(4, Zipped),
  %% {0, [{1, 2}, {2, 6}, {3, 10}, {4, 14}]}...
  Indexed = streams:with_index(Chunk),
  [{0, [{1, 2}, {2, 6}, {3, 10}, {4, 14}]}] = streams:to_list(Indexed),
  ok.

%% Private
expect_elems(List, Stream) ->
  Len = length(List),
  Finite = streams:take(Len, Stream),
  ?assertMatch(List, streams:to_list(Finite)).
