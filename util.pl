:- module(util, [set_day_input/1, file_input/1, input/1, lines/1, str/2]).
:- use_module(library(portray_text)).
:- dynamic day_input/1.
:- set_prolog_flag(double_quotes, codes).
:- set_portray_text(enabled, true).

set_day_input(InputString) :-
    retractall(day_input(_)),
    string_codes(InputString, Input),
    asserta(day_input(Input)).

file_input(Day) :-
    atomics_to_string([day,Day,'.txt'], Name),
    read_file_to_string(Name, Str, []),
    set_day_input(Str).

%% Parse input with the given DCG phrase
input(Phrase) :-
    day_input(Codes),
    phrase(Phrase, Codes, []).

line(L) --> string_without("\n", L), eol.
lines([]) --> eos.
lines([L|Ls]) --> line(L), lines(Ls).

% Read input file into lines
lines(Lines) :-
    input(lines(Lines)).

str(Thing, Str) :-
    with_output_to(string(Str), portray_clause(Thing)).

% Like convlist but calls pred on each successive head of the list
convheads(_, [], []).
convheads(Pred, List, [V|Vs]) :-
    call(Pred, List, V),
    [_|Rest] = List,
    convheads(Pred, Rest, Vs).

convheads(Pred, List, Vs) :-
    \+ call(Pred, List, _),
    [_|Rest] = List,
    convheads(Pred, Rest, Vs).

% Return successive heads of a list
heads([], []).
heads(Lst, [Lst|RestOfHeads]) :-
    [_|Rest] = Lst,
    heads(Rest, RestOfHeads).

split2(File, Split1, Split2, ParseItem, ListOfLists) :-
    read_file_to_string(File, Str, []),
    split_string(Str, Split1, "", Lines),
    convlist({Split2,ParseItem}/[Line,Items]>>(
                 \+ Line = "", % don't try to parse last empty line
                 split_string(Line, Split2, " ", ItemStrs),
                 maplist(ParseItem, ItemStrs, Items)),
             Lines, ListOfLists).
