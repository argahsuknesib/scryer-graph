:- module(reasoner, [solve/4, solve_with_rules/5]).
:- use_module(database).
:- use_module(library(lists)).

% --- Meta-Interpreter for Custom Rules ---
% solve_with_rules(Subject, Predicate, Object, Graph, Rules)

% 1. Check if the fact exists in the Database
solve_with_rules(S, P, O, G, _Rules) :-
    database:rdf(S, P, O, G).

% 2. Apply Custom Rules
solve_with_rules(S, P, O, G, Rules) :-
    member(rule(rdf(S, P, O), Body), Rules),
    solve_body(Body, G, Rules).

solve_body([], _, _).
solve_body([rdf(S, P, O)|Rest], G, Rules) :-
    solve_with_rules(S, P, O, G, Rules),
    solve_body(Rest, G, Rules).

% --- Default Hardcoded Reasoner (Keeping existing logic) ---
% Base Fact : There exists a triple (Subject, Predicate, Object, Graph) in the database.
solve(Subject,Predicate,Object,Graph) :-
    database:rdf(Subject,Predicate,Object,Graph).


% Inverse Properties
solve(Subject,Predicate2,Object,Graph) :- 
  database:rdf(Predicate1,"inverseOf",Predicate2,Graph),
  solve(Object,Predicate1,Subject,Graph).

% Symmetric Properties
solve(Subject,Predicate,Object,Graph) :-
  database:rdf(Predicate,"type","SymmetricProperty",Graph),
  database:rdf(Object,Predicate,Subject,Graph).

% SubPropertyOf
solve(Subject,Predicate2,Object,Graph) :-
  database:rdf(Predicate1,"subPropertyOf",Predicate2,Graph),
  solve(Subject,Predicate1,Object,Graph).

% SubClassOf
solve(Subject,"type",Class2,Graph) :-
  database:rdf(Class1,"subClassOf",Class2,Graph),
  solve(Subject,"type",Class1,Graph).

% Transitive Properties
% If Predicate is transitive, Subject-Predicate-Intermediate and Intermediate-Predicate-Object then Subject-Predicate-Object
solve(Subject,Predicate,Object,Graph) :-
 database:rdf(Predicate,"type","TransitiveProperty",Graph),
 solve(Subject,Predicate,Intermediate,Graph),
 solve(Intermediate,Predicate,Object,Graph).