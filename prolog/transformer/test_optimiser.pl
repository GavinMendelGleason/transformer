:- module(test_optimiser,[]).

:- use_module(transform_optimise).

:- optimise(p/3).

p(x,y,z) :- q.
p(x,w,z) :- r.
p(x,y,q) :- s.

p(x,y) :- s.
p(z,w) :- s.

q.
q.

r.

s.


