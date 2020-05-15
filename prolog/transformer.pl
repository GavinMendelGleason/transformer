:- module(transformer, [
              % optimise.pl
              optimise/1,
              optimise_all/0,
              optimise_everything/0,
              set_optimise_options/1,
              % reify.pl
              reify_reflect/2,
              predicate_clauses/3,
              declarations/2
          ]).

:- use_module(transformer/reify).
:- use_module(transformer/equality_optimise).
:- use_module(library(option)).

:- dynamic '$program'/1.
:- thread_local '$program'/1.
:- dynamic '$participating_clause'/1.
:- thread_local '$participating_clause'/1.
:- dynamic '$options'/1.

:- op(920,fy, *).
*_.

'$options'([equality_cut(true), log_stream(current_output)]).

optimise(Predicate) :-
    assertz('$participating_clause'(Predicate)).

optimise_all :-
    assertz('$participating_clause'(_)).

optimise_everything :-
    set_optimise_options([optimise_everything(true)]).

module_wants_transformer(_Module) :-
    optimise_everything,
    !.
module_wants_transformer(Module) :-
    Module \= transformer,
    predicate_property(Module:optimise(_), imported_from(transformer)).

participating_clause(Clause) :-
    Clause = clause(Pred,_,_),
    (   '$participating_clause'(Pred)
    ->  true
    ;   check_option(optimise_everything(true))).

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
    memberchk(Option,Options).

report_optimisation(Pred) :-
    check_option(log_stream(Stream)),
    format(Stream, '~nTransformer: Performed optimisation on ~q~n', [Pred]).

optimise_clauses(Clauses, Optimised) :-
    check_option(equality_cut(true)),
    equality_optimise(Clauses,Optimised),
    (   Clauses = Optimised
    ->  true
    ;   Clauses = [clause(Pred,_,_)|_Rest],
        report_optimisation(Pred)).

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
    transformer:module_wants_transformer(Module),
    retractall(transformer:'$program'(_)),
    retractall(transformer:'$participating_clause'(_)).
user:term_expansion(end_of_file,Terms) :-
    prolog_load_context(module, Module),
    transformer:module_wants_transformer(Module),
    transformer:transformed_terms(Terms).
user:term_expansion(Term, No_Clause) :-
    \+ memberchk(Term, [begin_of_file, end_of_file]),
    prolog_load_context(module, Module),
    transformer:module_wants_transformer(Module),
    transformer:reify_reflect(Term,Statement),
    transformer:participating_clause(Statement),
    assertz(transformer:'$program'(Statement)),
    No_Clause = [].
