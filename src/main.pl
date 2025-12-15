% Importing modules for parsing and database operations
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(lists)). % Added for length/2
:- use_module(database).
:- use_module(parser).
:- use_module(reasoner).
:- use_module(rule_loader).

% Main REPL loop
start :-
  format("Welcome to Scryer Graph Database!~nType 'quit' or 'exit' to leave.~n", []),
  repl_loop([]).

repl_loop(Rules) :-
  format("scryer-graph> ", []),
  read_input_line(Line),
  ( Line = end_of_file_token ->
      run(quit, Rules)
  ; catch(parser:parse_command(Line, Command), Error, (format("Error parsing command: ~w - ~s~n", [Error, Line]), Command = error)),
    ( Command = error -> repl_loop(Rules) % Loop again with same rules on error
    ; run(Command, Rules)
    )
  ).

% Helper predicate to read a line of input and handle EOF
read_input_line(Line) :-
    read_line_to_chars(current_input, Chars),
    ( Chars = end_of_file ->
        Line = end_of_file_token
    ; Line = Chars
    ).

run(quit, _) :-
  format("Goodbye!~n", []).

run(add(Subject,Predicate,Object,Graph), Rules) :-
  database:add_graph(Subject,Predicate,Object,Graph),
  format("Added the RDF Graph.~n", []),
  repl_loop(Rules).

% Standard query using the default hardcoded reasoner (or base DB)
run(query(Subject,Predicate,Object,Graph), Rules) :-
  findall([S,P,O,G], (
    (Subject = "who" -> true ; S = Subject),
    (Predicate = "who" -> true ; P = Predicate),
    (Object = "who" -> true ; O = Object),
    (Graph = "who" -> true ; G = Graph),
    reasoner:solve(S, P, O, G)
  ), Results),
  (Results = [] -> format("No matching RDF triples found.~n", []) ;
   (format("Results:~n", []), print_results(Results))),
  repl_loop(Rules).

% Custom reasoning using loaded rules
run(reason(Subject,Predicate,Object,Graph), Rules) :-
  findall([S,P,O,G], (
    (Subject = "who" -> true ; S = Subject),
    (Predicate = "who" -> true ; P = Predicate),
    (Object = "who" -> true ; O = Object),
    (Graph = "who" -> true ; G = Graph),
    reasoner:solve_with_rules(S, P, O, G, Rules)
  ), Results),
  (Results = [] -> format("No matching RDF triples found using custom rules.~n", []) ;
   (format("Results:~n", []), print_results(Results))),
  repl_loop(Rules).

run(save(Filename), Rules) :-
  database:save_database(Filename),
  format("Database saved to ~w.~n", [Filename]),
  repl_loop(Rules).

run(load(Filename), Rules) :-
  database:load_database(Filename),
  format("Database loaded from ~w.~n", [Filename]),
  repl_loop(Rules).

run(load_rules(Filename), _) :-
  catch(rule_loader:load_rules_from_file(Filename, NewRules), Error, 
        (format("Error loading rules: ~w~n", [Error]), NewRules = [])),
  length(NewRules, Len),
  format("Loaded ~d rules from ~w.~n", [Len, Filename]),
  repl_loop(NewRules).

print_results([]).
print_results([T|Ts]) :- print_triple(T), print_results(Ts).

print_triple([S,P,O,G]) :-
  format("  ~s ~s ~s ~s~n", [S,P,O,G]).

:- initialization(start).