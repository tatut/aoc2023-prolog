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

file(File, Str) :- read_file_to_string(File, Str, []).

split2_matches([], []).
split2_matches([_Skip], []).
split2_matches([_Skipped,Match|Rest], [Match|Matches]) :- split2_matches(Rest, Matches).

split2(Str, Re1, Re2, ListOfLists) :-
    re_split(Re1, Str, Matches), split2_matches(Matches, Lines),
    maplist({Re2}/[Line,Items]>>( re_split(Re2, Line, Ms), split2_matches(Ms, Items) ),
            Lines, ListOfLists).

enumerate(_,[],[]).
enumerate(I,[X|Xs],[I-X|Rest]) :- I1 is I + 1, enumerate(I1, Xs,Rest).
enumerate(Lst,Enumerated) :- enumerate(1, Lst, Enumerated).

counts(List, Counts) :- msort(List, Items), clumped(Items, Counts).

repeat(1, _, _, Done, Done).
repeat(N, Sep, Items, Acc, Done) :-
    N > 1, succ(N1, N),
    append(Acc,Sep,Acc1),
    append(Acc1,Items,Acc2),
    repeat(N1, Sep, Items, Acc2, Done).

repeat(Times, Lst, Repeated) :- repeat(Times, [], Lst, Lst, Repeated).
repeat(Times, Sep, Lst, Repeated) :- repeat(Times, Sep, Lst, Lst, Repeated).
