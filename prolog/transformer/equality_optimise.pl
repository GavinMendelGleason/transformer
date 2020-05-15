:- module(equality_optimise, [
              equality_optimise/2
          ]).

:- use_module(library(plunit)).

/*
 * Introduces a cut when subsequent clauses are impossible based on equality constraints.
 */
equality_disjoint_arguments([],_Args).
equality_disjoint_arguments([clause(_,Alt_Args,_)|Remainder],Args) :-
    \+ Args = Alt_Args,
    equality_disjoint_arguments(Remainder,Args).

clauses_clause_transformed([],Clause,[Clause]).
clauses_clause_transformed([First|Remainder],clause(Pred,Args,Body),[clause(Pred,Args,New_Body)|Clauses]) :-
    (   equality_disjoint_arguments([First|Remainder],Args)
    ->  New_Body = [(!)|Body]
    ;   New_Body = Body),
    clauses_clause_transformed(Remainder,First,Clauses).

equality_optimise([],[]).
equality_optimise([Clause|Predicate_Clauses],Transformed_Clauses) :-
    clauses_clause_transformed(Predicate_Clauses,Clause,Transformed_Clauses).


:- begin_tests(equality_optimise).

test(cuting_clause, []) :-

    Clauses = [clause(p/3,[x,y,z],[q]),
               clause(p/3,[x,w,z],[r]),
               clause(p/3,[x,y,q],[s])],
    equality_optimise(Clauses,Trans),

    Trans = [clause(p/3,[x,y,z],[!,q]),
             clause(p/3,[x,w,z],[!,r]),
             clause(p/3,[x,y,q],[s])].


test(cut_later, []) :-

    Clauses = [clause(p/3,[x,_,_],[q]),
               clause(p/3,[x,y,z],[r]),
               clause(p/3,[x,y,q],[s])],
    equality_optimise(Clauses,Trans),

    Trans = [clause(p/3,[x,_,_],[q]),
             clause(p/3,[x,y,z],[!,r]),
             clause(p/3,[x,y,q],[s])].

test(cut_never, []) :-

    Clauses = [clause(p/3,[x,_,_],[q]),
               clause(p/3,[_,y,_],[r]),
               clause(p/3,[_,_,z],[s])],
    equality_optimise(Clauses,Trans),
    Trans = [clause(p/3,[x,_,_],[q]),
             clause(p/3,[_,y,_],[r]),
             clause(p/3,[_,_,z],[s])].


test(cut_dict, []) :-

    Clauses = [clause(p/3,[a{x : X},_],[q]),
               clause(p/3,[a{y : X},y],[r]),
               clause(p/3,[b{z : X},_],[s])],
    equality_optimise(Clauses,Trans),
    Trans = [clause(p/3,[a{x:_},_],[!,q]),
             clause(p/3,[a{y:_},y],[!,r]),
             clause(p/3,[b{z:_},_],[s])].


test(useless, []) :-

    Clauses = [clause(p/3,[X],[q]),
               clause(p/3,[X],[r]),
               clause(p/3,[X],[s])],
    equality_optimise(Clauses,Trans),
    Trans = [clause(p/3,[_],[q]),
             clause(p/3,[_],[r]),
             clause(p/3,[_],[s])].

:- end_tests(equality_optimise).
