:- use_module(library(dcg/basics)).

values([]) --> eos.
values([V|Vs]) --> string_without(`\n`, V), eol, values(Vs).

in(I) :- phrase_from_file(values(I), "day1.txt").

digits([], []).
digits([C|Cs], [D|Ds]) :- code_type(C, digit), D is C - 48, digits(Cs, Ds), !.
digits([_|Cs], Ds) :- digits(Cs, Ds).

calibration_value(Digits, Cs, V) :-
    call(Digits, Cs, Ds),
    [First|_] = Ds,
    last(Ds, Last),
    V is First * 10 + Last.

part(Digits, Ans) :-
    in(In),
    maplist(calibration_value(Digits), In, Vs),
    sum_list(Vs, Ans).

part1(A) :- part(digits, A).

spelled(0, `zero`).
spelled(1, `one`).
spelled(2, `two`).
spelled(3, `three`).
spelled(4, `four`).
spelled(5, `five`).
spelled(6, `six`).
spelled(7, `seven`).
spelled(8, `eight`).
spelled(9, `nine`).

digits2([], []).
digits2([C|Cs], [D|Ds]) :- code_type(C, digit), D is C - 48, digits2(Cs, Ds), !.
digits2(Cs, [D|Ds]) :- spelled(D, NumCs),
                       append(NumCs, _, Cs),
                       [_|RestCs] = Cs,
                       digits2(RestCs, Ds), !.
digits2([_|Cs], Ds) :- digits2(Cs, Ds).

part2(A) :- part(digits2, A).
