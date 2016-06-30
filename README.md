streams
=====

Lazy lists in Erlang.

Obtaining the first 10 squares of odd natural numbers:
```
Eshell V7.3  (abort with ^G)
1> Odds = streams:filter(fun(A) -> A rem 2 > 0 end, streams:naturals()),
1> Squares = streams:map(fun(A) -> A * A end, Odds),
1> Take10 = streams:take(10, Squares),
1> streams:to_list(Take10).
[1,3,5,7,9,11,13,15,17,19]
```

Build
-----

    $ rebar3 compile
