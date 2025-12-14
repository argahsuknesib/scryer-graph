
% Importing modules for parsing and database operations
:- use_module(library(format)).
:- use_module(database).

% Main REPL loop
start :-
  format("Welcome to Scryer Graph Database!~nType 'quit' or 'exit' to leave.~n", []),
  repl_loop.

repl_loop :-
  format("scryer-graph> ", []),
  catch(read(Command), Error, (format("Error reading input: ~w~n", [Error]), Command = quit)),
  ( Command = end_of_file ; Command = quit ->
      format("Goodbye!~n", [])
  ; run(Command),
    repl_loop
  ).

run(quit) :-
  format("Goodbye!~n", []).

run(add(Subject,Predicate,Object,Graph)) :-
  database:add_graph(Subject,Predicate,Object,Graph),
  format("Added the RDF Graph.~n", []),
  repl_loop.

run(query(Subject,Predicate,Object,Graph)) :-
  findall([S,P,O,G], (
    (Subject = "who" -> true ; S = Subject),
    (Predicate = "who" -> true ; P = Predicate),
    (Object = "who" -> true ; O = Object),
    (Graph = "who" -> true ; G = Graph),
    database:query_graph(S, P, O, G)
  ), Results),
  (Results = [] -> format("No matching RDF triples found.~n", []) ;
   (format("Results:~n", []), print_results(Results))),
  repl_loop.

run(save(Filename)) :-
  database:save_database(Filename),
  format("Database saved to ~w.~n", [Filename]),
  repl_loop.

run(load(Filename)) :-
  database:load_database(Filename),
  format("Database loaded from ~w.~n", [Filename]),
  repl_loop.

print_results([]).
print_results([T|Ts]) :- print_triple(T), print_results(Ts).

print_triple([S,P,O,G]) :-
  format("  ~s ~s ~s ~s~n", [S,P,O,G]).

:- initialization(start).

% Entry point
% To run the REPL, load this file in Scryer Prolog.
% Then call the predicate start/0.
% Example commands:
% ?- start.
% scryer-graph> add("Alice", "knows", "Bob", "social").
% scryer-graph> query("Alice", "knows", "who", "social").
% scryer-graph> save("backup.txt").
% scryer-graph> load("backup.txt").
% scryer-graph> quit.
% Goodbye!
