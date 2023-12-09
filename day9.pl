lines(I, Ls) :- re_split("\n", I, Lines), convlist([L,L]>>(\+ L = "\n"), Lines, Ls).
numbers(L, Ns) :- re_split(" +", L, Nums),
                  convlist([S,N]>>number_string(N,S), Nums, Ns).
histories(Str, Hs) :-
    lines(Str, Ls),
    maplist(numbers, Ls, Hs).

input(Hs) :- read_file_to_string('day9.txt', Str, []), histories(Str, Hs).

deltas([_],[]).
deltas([A,B|Cs], [D|Rest]) :-
    D is B - A,
    deltas([B|Cs], Rest).

zeroes(Ls) :- maplist(=(0), Ls).

predict(Zs, 0) :- zeroes(Zs).
predict(Ls, P) :-
    \+ zeroes(Ls),
    deltas(Ls, Ds),
    predict(Ds, Dsp),
    last(Ls, Last),
    P is Last + Dsp.

part(Predict, Ans) :- input(Hs), maplist(Predict, Hs, Ps), sum_list(Ps, Ans).

part1(Ans) :- part(predict, Ans).

% Part2 predict before
%% predict2(Zs, 0) :- zeroes(Zs).
%% predict2(Ls, P) :-
%%     \+ zeroes(Ls),
%%     deltas(Ls, Ds),
%%     predict2(Ds, Dsp),
%%     Ls=[F|_],
%%     P is F - Dsp.

% Better version, just reverse the list and run normal prediction
predict2(Ls, P) :- reverse(Ls, Rs), predict(Rs, P).

part2(Ans) :- part(predict2, Ans).
% part2(903).
