:- module(rule_loader, [load_rules_from_file/2]).
:- use_module(library(iso_ext)). % For open/4 etc if needed, but built-ins are usually fine.
:- use_module(library(lists)).
:- use_module(library(charsio)).

load_rules_from_file(Filename, Rules) :-
    atom_chars(Fn, Filename),
    open(Fn, read, Stream),
    read(Stream, Term),
    read_rules(Stream, Term, Rules),
    close(Stream).

read_rules(_, end_of_file, []) :- !.
read_rules(Stream, Term, [Rule|Rules]) :-
    normalize_rule(Term, Rule),
    read(Stream, NextTerm),
    read_rules(Stream, NextTerm, Rules).

% Convert Prolog term 'Head :- Body' to rule(HeadTriple, BodyTriplesList)
normalize_rule(:-(Head, Body), rule(H, B)) :-
    term_to_triple(Head, H),
    body_to_list(Body, B).

% Fact rule: 'Head.' -> rule(HeadTriple, [])
normalize_rule(Head, rule(H, [])) :-
    Head \= :-(_, _),
    Head \= end_of_file,
    term_to_triple(Head, H).

% Convert term like ancestor(X,Y) to rdf(X, "ancestor", Y)
term_to_triple(Term, rdf(S, P, O)) :-
    Term =.. [PredAtom, S, O],
    atom_chars(PredAtom, P).

% Convert conjunction (A, B) to list [A, B]
body_to_list((Goal, Rest), [G|Gs]) :-
    term_to_triple(Goal, G),
    body_to_list(Rest, Gs).
body_to_list(Goal, [G]) :-
    Goal \= (_, _),
    term_to_triple(Goal, G).
