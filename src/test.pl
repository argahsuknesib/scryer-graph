:- use_module('database').
:- use_module('reasoner').
:- use_module('rule_loader').
:- use_module(library(lists)).
:- use_module(library(format)).

% Test adding triples
test_add :-
    add_graph("Alice", "knows", "Bob", "social"),
    add_graph("Bob", "knows", "Charlie", "social"),
    add_graph("Abe", "parent", "Homer", "simpsons"),
    add_graph("Homer", "parent", "Bart", "simpsons"),
    add_graph("Homer", "parent", "Lisa", "simpsons"),
    add_graph("knows", "type", "TransitiveProperty", "social"),
    format("Added test triples.~n", []).

% Test querying direct facts
test_query :-
    query_graph("Alice", "knows", "Bob", "social"),
    format("Query successful: Alice knows Bob.~n", []).

% Test Reasoning
test_reasoning :-
    % 1. Hardcoded Reasoner (Transitivity)
    % Alice knows Bob, Bob knows Charlie, knows is Transitive -> Alice knows Charlie
    findall(O, reasoner:solve("Alice", "knows", O, "social"), KnownPeople),
    member("Charlie", KnownPeople),
    format("Hardcoded Reasoner passed: Alice knows Charlie (Transitive).~n", []),

    % 2. Meta-Interpreter (Custom Rules)
    rule_loader:load_rules_from_file("test_rules.pl", Rules),

    % Test Ancestor Rule (Recursive)
    % Abe -> Homer -> Bart. Abe should be ancestor of Bart.
    findall(Descendant, reasoner:solve_with_rules("Abe", "ancestor", Descendant, "simpsons", Rules), Descendants),
    member("Bart", Descendants),
    format("Dynamic Reasoner passed: Abe is ancestor of Bart.~n", []),

    % Test Sibling Rule
    % Homer -> Bart, Homer -> Lisa. Bart and Lisa should be siblings.
    findall(Sib, reasoner:solve_with_rules("Bart", "sibling", Sib, "simpsons", Rules), Siblings),
    member("Lisa", Siblings),
    format("Dynamic Reasoner passed: Bart has sibling Lisa.~n", []).

% Run all tests
run_tests :-
    test_add,
    test_query,
    test_reasoning,
    format("All tests passed.~n", []).

% To run tests, load this file and query: ?- run_tests.
