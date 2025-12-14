:- use_module('database').

% Test adding triples
test_add :-
    add_graph("Alice", "knows", "Bob", "social"),
    add_graph("Bob", "knows", "Charlie", "social"),
    write('Added test triples.'), nl.

% Test querying
test_query :-
    query_graph("Alice", "knows", "Bob", "social"),
    write('Query successful: Alice knows Bob.'), nl.



% Run all tests
run_tests :-
    test_add,
    test_query,
    write('All tests passed.'), nl.

:- run_tests, halt.
