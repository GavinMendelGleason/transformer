:- module(test_optimiser,[]).

:- use_module(library(transformer)).

:- optimise(p/3).

p(x,y,z) :- q.
p(x,w,z) :- r.
p(x,y,q) :- s.

p(x,y) :- s.
p(z,w) :- s.

tau(z) :- q.
tau(w) :- r.

:- optimise_all.

q.
q.

r.
r.

s.
s.

