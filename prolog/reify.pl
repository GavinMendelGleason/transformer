:- module(reify, [
              reify_reflect/2,
              bodyless_term/1,
              predicate_clauses/3
          ]).

/*
 * xfy_list(Op, Term, List) is det.
 *
 * Folds a functor over a list.
 */
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).

/*
 * reify_reflect(+,-) is det.
 * reify_reflect(-,+) is det.
 */
reify_reflect((:- Body),declaration(Clauses)) :-
    xfy_list(',', Body, Clauses).
reify_reflect((Head --> Body),clause(P//N,Args,Clauses)) :-
    Head =.. [P|Args],
    length(Args,N),
    xfy_list(',', Body, Clauses).
reify_reflect((Head :- Body),clause(P/N,Args,Clauses)) :-
    Head =.. [P|Args],
    length(Args,N),
    xfy_list(',', Body, Clauses).
reify_reflect(Head,clause(P/N,Args,[])) :-
    Head =.. [P|Args],
    length(Args,N).

/* Pred is P/N or P//N
 */
predicate_clauses(Program,Pred,Clauses) :-
    bagof(
        Clause,
        (   member(Clause,Clauses),
            Clause = clause(Pred,_,_)),
        Clauses).

declarations(Program,Declarations) :-
    bagof(
        Declaration,
        (   member(Declartion,Program),
            Declartion = declaration(_)),
        Declarations).

/*

cd swipl-devel
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=$HOME -G Ninja ..
ninja
ctest -j 4
ninja install

*/
