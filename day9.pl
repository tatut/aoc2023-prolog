:- use_module(util).

histories(Hs) :- util:file('day9.txt', S), util:split2(S, ".+\n", "-?\\d+"/n, Hs).

deltas([_],[]).
deltas([A,B|Cs], [D|Rest]) :- D is B - A, deltas([B|Cs], Rest).

zeroes(Ls) :- maplist(=(0), Ls).

predict(Zs, 0) :- zeroes(Zs).
predict(Ls, P) :-
    \+ zeroes(Ls),
    deltas(Ls, Ds),
    predict(Ds, Dsp),
    last(Ls, Last),
    P is Last + Dsp.

part(Predict, Ans) :- histories(Hs), maplist(Predict, Hs, Ps), sum_list(Ps, Ans).

part1(Ans) :- part(predict, Ans).
% part1(1842168671).

% Part2 predict before
predict2(Ls, P) :- reverse(Ls, Rs), predict(Rs, P).

part2(Ans) :- part(predict2, Ans).
% part2(903).
