:- module(optmise, [
              optimise/1
          ]).


:- use_module(reify).
:- use_module(equality_optimise).

:- dynamic '$program'/1.
:- dynamic '$participating_clause'/1.

optimise(Predicate) :-
    assertz('$participating_clause',Predicate).

module_wants_guard(Module) :-
    Module \= optimise,
    predicate_property(Module:optimise(_), imported_from(optimise)).

participating_predicates(Pred,Clauses) :-
    member(declaration(optimise(Pred)), Clauses).

transformed_clauses(Clauses) :-
    participating_predicates(Pred,Clauses),
    true.


user:term_expansion(begin_of_file, begin_of_file) :-
    % We don't want to process ourselves, so fail...
    prolog_load_context(module, Module),
    optimise:module_wants_guard(Module),
    retractall(optimise:'$program'(_)),
    retractall(optimise:'$participating_clause'(_)).
user:term_expansion(end_of_file,Clauses) :-
    prolog_load_context(module, Module),
    optimise:module_wants_guard(Module),
    optimise:transformed_clauses(Clauses),
    optimise:give_report.
user:term_expansion(Term, No_Clause) :-
    prolog_load_context(module, Module),
    optimise:module_wants_guard(Module),
    optimise:reify_reflect(Statement,Term),
    assertz(optimise:'$program'(Statement)),
    No_Clause = [].
