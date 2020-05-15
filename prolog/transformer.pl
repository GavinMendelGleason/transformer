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

:- use_module(transformer/optimise).
:- use_module(transformer/reify).
