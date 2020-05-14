# transformer

Tools for program transformation in prolog.

## Optimiser

The optimiser is intended to give a suite of optimisations that can be
performed on a file wide, or per-predicate basis.

Currently the only optimisation method deployed is a search for
clause-overlap which inserts cuts when we can prove that subsequent
clauses are unreachable.

To start the optimiser on a predicate:

```prolog
:- use_module(library(transformer)).

:- optimise(p/3).
p(x,y,z) :- q.
p(x,w,z) :- r.
p(x,y,a) :- s.
```

or run it on the whole file: 

```prolog
:- use_module(library(transformer)).
:- optimise_all.
```

# TODO

* A "guard" optimiser which does a search for all predicates at the
  beginning of a clause which are in a guard language
  (integer,string,atom,var,nonvar,ground etc.) and places a cut when
  subsequent clauses are unreachable.
* Negation elimination. Cuts in previous clauses tell us that in order
  to have cascaded, something before the cut must have been false. i.e

```prolog

p(X) :-
  nonvar(X),
  int(X),
  !,
  G.
p(X) :-
  H.

  % implies that
  \+ (nonvar(X),
      int(X)) in H.
```
