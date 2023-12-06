:- use_module(library(clpfd)).
input([53-313, 89-1090, 76-1214, 98-1201]).
input2(53897698-313109012141201).

moved(Duration, ToBeat, PressTime, Moved) :-
    Moved #> ToBeat,
    PressTime #< Duration,
    Moved #= PressTime * (Duration - PressTime).

ways_to_beat(Duration-ToBeat, Ways) :-
    moved(Duration, ToBeat, PressMin, M),
    moved(Duration, ToBeat, PressMax, M),
    labeling([min(PressMin),max(PressMax)], [PressMin,PressMax]),
    Ways #= PressMax - PressMin + 1.

part1(Ans) :- % 5133600
    input(Races),
    maplist(ways_to_beat, Races, Ways),
    foldl([A,B,C]>>(C #= A * B), Ways, 1, Ans).

part2(Ans) :- % 40651271
    input2(Duration-ToBeat),
    ways_to_beat(Duration-ToBeat, Ans).

quadratic(B,C, S1, S2) :-
    % A is one and omitted
    S1 is (B - sqrt(B^2 - 4 * C)) / 2,
    S2 is (B + sqrt(B^2 - 4 * C)) / 2.

ways_to_beat_q(Duration-ToBeat, Ways) :-
    quadratic(Duration, ToBeat, S1, S2),
    Ways is floor(S2) - ceil(S1) + 1.

part1q(Ans) :- % 5133600
    input(Races),
    maplist(ways_to_beat_q, Races, Ways),
    foldl([A,B,C]>>(C #= A * B), Ways, 1, Ans).

part2q(Ans) :- input2(I), ways_to_beat_q(I, Ans).
