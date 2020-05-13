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

participating_predicate(Pred,Program) :-
    predicate_clauses(Program, Pred, _Clauses).

get_program(Program) :-
    bagof(Clause,
          '$program'(Clause),
          Program).

transformed_clauses(Clauses) :-
    get_program(Program),
    findall(New_Clauses,
            (   member(Clause, Program)
            ->  (   predicate_clauses(Program, Pred, Predicate_Clauses) ->
                    (   participating_predicates(Pred,Program)
                    ->  optimise_clauses(Predicate_Clauses,New_Clauses)
                    ;   Predicate_Clauses = New_Clauses)
                ;   New_Clauses = Predicate_Clauses)
            ;   New_Clauses = true % huh?
            Clauses).


user:term_expansion(begin_of_file, begin_of_file) :-
    % We don't want to process ourselves, so fail...
    prolog_load_context(module, Module),
    optimise:module_wants_guard(Module),
    retractall(optimise:'$program'(_)),
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
