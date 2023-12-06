:- use_module(library(clpfd)).

testinput([7-9, 15-40, 30-200]).
input([53-313, 89-1090, 76-1214, 98-1201]).

moved(Duration, ToBeat, PressTime, Moved) :-
    Moved #> ToBeat,
    PressTime #< Duration,
    Moved #= PressTime * (Duration - PressTime).

ways_to_beat(Duration-ToBeat, Ways) :-
    moved(Duration, ToBeat, PressMin, M),
    moved(Duration, ToBeat, PressMax, M),
    labeling([min(PressMin),max(PressMax)], [PressMin,PressMax]),
    Ways #= PressMax - PressMin + 1.

mult(A,B,C) :- C is A * B.

part1(Ans) :-
    input(Races),
    maplist(ways_to_beat, Races, Ways),
    foldl(mult, Ways, 1, Ans).

% part1(5133600).

% Part2

testinput2(71530-940200).
input2(53897698-313109012141201).

part2(Ans) :-
    input2(Duration-ToBeat),
    ways_to_beat(Duration-ToBeat, Ans).

% part2(40651271).
