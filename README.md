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

A motivating example of laziness in action:
```
Eshell V7.3  (abort with ^G)
1> Candidates = streams:iterate(fun(I) -> I + 1 end, 2),
1>
1> PrimeGen = fun FilterPrimes(Stream) ->
1>    streams:lazily(fun(P, Xs) ->
1>      {P, FilterPrimes(streams:filter(fun(Y) -> Y rem P =/= 0 end, Xs))}
1>    end, Stream)
1> end,
1> Primes = PrimeGen(Candidates),
1> First10Primes = streams:take(10, Primes),
1> streams:to_list(First10Primes).
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```

Build
-----

    $ rebar3 compile
