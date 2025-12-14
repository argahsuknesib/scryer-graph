:- module(database, [add_graph/4, query_graph/4, save_database/1, load_database/1]).
:- use_module(library(lists)).
:- use_module(library(format)).



% The state of the database allowing rdf to be modified at runtime.
:- dynamic(rdf/4).

% API to use
add_graph(Subject,Predicate,Object,Graph) :- assertz(rdf(Subject,Predicate,Object,Graph)).
query_graph(Subject,Predicate,Object,Graph) :- rdf(Subject,Predicate,Object,Graph).
save_database(Filename) :-
    atom_chars(Fn, Filename),
    setup_call_cleanup(
      open(Fn, write, Stream),
      (
      format(Stream, "Persistance Backup~n", []),
      forall(rdf(Subject,Predicate,Object,Graph),format(Stream, "rdf(~q,~q,~q,~q).~n",[Subject,Predicate,Object,Graph]))
      ),
      close(Stream)
    ).

load_database(Filename) :- atom_chars(Fn, Filename),
  load_files(Fn, [module(database)]).
