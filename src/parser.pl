:- module(parser, [parse_command/2]).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

parse_command(InputString, Command) :-
  phrase(command(Command), InputString).

% Different Commands
command(quit) --> "quit".
command(quit) --> "exit".
command(save(F)) --> "save", ws, name_str(F).
command(load(F)) --> "load", ws, name_str(F).
command(load_rules(F)) --> "load_rules", ws, name_str(F).
command(add(Subject,Predicate,Object,Graph)) --> "add", ws, term(Subject), ws, term(Predicate), ws, term(Object), ws, term(Graph).
command(query(Subject,Predicate,Object,Graph)) --> term(Subject), ws, term(Predicate), ws, term(Object), ws, term(Graph).
command(reason(Subject,Predicate,Object,Graph)) --> "reason", ws, term(Subject), ws, term(Predicate), ws, term(Object), ws, term(Graph).

% Parsing Terms
term(Term) --> "null", { Term = null }.
term(Term) --> integer_str(IntStr), { number_chars(Term, IntStr) }.
term(Term) --> quoted_str(QStr), { Term = QStr }.
term(Term) --> name_str(Name), { Term = Name }.

% Parsing Strings
ws --> " ", ws | [].
name_str([C|Cs]) --> [C], { C \= ' ', C \= '\n', C \= '"' }, name_str(Cs).
name_str([]) --> [].

% Integer string: sequence of digits
integer_str([D|Ds]) --> [D], { char_type(D, digit) }, integer_str(Ds).
integer_str([]) --> [].

% Quoted string: " chars "
quoted_str(QStr) --> "\"", quoted_chars(Chars), "\"", { QStr = Chars }.
quoted_chars([C|Cs]) --> [C], { C \= '"' }, quoted_chars(Cs).
quoted_chars([]) --> [].
