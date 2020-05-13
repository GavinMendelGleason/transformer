:- module(equality_optimise, [
              equality_optimise(Predicate_Clauses,Transformed_Clauses)
          ]).

:- use_module(library(plunit)).

/*
 * Introduces a cut when subsequent clauses are impossible based on equality constraints.
 */
arguments_equality_disjoint(_Args,[]).
arguments_equality_disjoint(Args,[clause(_,Alt_Args,_)|Remainder]) :-
    \+ Args = Alt_Args,
    arguments_equality_disjoint(Args,Remainder).

clauses_clause_transformed([],Clause,[Clause]).
clauses_clause_transformed([First|Remainder],clause(Pred,Args,Body),[clause(Pred,Args,New_Body)|Clauses]) :-
    (   arguments_equality_disjoint(Args,[First|Remainder])
    ->  New_Body = [(!)|Body]
    ;   New_Body = Body),
    clause_clauses_transformed(Remainder,First,Clauses).

