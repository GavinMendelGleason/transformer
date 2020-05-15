:- module(optimise, [
              optimise/1,
              optimise_all/0,
              set_optimise_options/1
          ]).

:- use_module(reify).
:- use_module(equality_optimise).
:- use_module(library(option)).

:- dynamic '$program'/1.
:- thread_local '$program'/1.
:- dynamic '$participating_clause'/1.
:- dynamic '$options'/1.

:- op(920,fy, *).
*_.

'$options'([equality_cut(true)]).

optimise(Predicate) :-
    assertz('$participating_clause'(Predicate)).

optimise_all :-
    assertz('$participating_clause'(_)).

module_wants_optimise(Module) :-
    Module \= optimise,
    predicate_property(Module:optimise(_), imported_from(optimise)).

participating_clause(Clause) :-
    Clause = clause(Pred,_,_),
    '$participating_clause'(Pred).

get_program(Program) :-
    bagof(Clause,
          '$program'(Clause),
          Program).

set_optimise_options(Options) :-
    get_optimise_options(Old_Options),
    merge_options(Options,Old_Options,New_Options),
    retractall('$options'(_)),
    assertz('$options'(New_Options)).

get_optimise_options(Options) :-
    '$options'(Options).

check_option(Option) :-
    get_optimise_options(Options),
    member(Option,Options).

optimise_clauses(Clauses, Optimised) :-
    check_option(equality_cut(true)),
    equality_optimise(Clauses,Optimised).

transformed_terms(Terms) :-
    get_program(Program),
    findall(New_Terms,
            (   predicate_clauses(Program, _Pred, Predicate_Clauses),
                optimise_clauses(Predicate_Clauses,New_Clauses),
                maplist(reify_reflect,New_Terms,New_Clauses)
            ),
            Terms_Sets),
    append(Terms_Sets,Terms).

user:term_expansion(begin_of_file, begin_of_file) :-
    % We don't want to process ourselves, so fail...
    prolog_load_context(module, Module),
    optimise:module_wants_optimise(Module),
    retractall(optimise:'$program'(_)),
    retractall(optimise:'$participating_clause'(_)).
user:term_expansion(end_of_file,Terms) :-
    prolog_load_context(module, Module),
    optimise:module_wants_optimise(Module),
    optimise:transformed_terms(Terms).
user:term_expansion(Term, No_Clause) :-
    prolog_load_context(module, Module),
    optimise:module_wants_optimise(Module),
    optimise:reify_reflect(Term,Statement),
    participating_clause(Statement),
    assertz(optimise:'$program'(Statement)),
    No_Clause = [].
