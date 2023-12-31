:- use_module(util).
:- table configs/3.

parse_row(Str, Line-Damaged) :-
    split_string(Str, " ", "", [LinePart,DmgPart]),
    string_codes(LinePart, Line),
    re_split("\\d+"/n, DmgPart, Dmgs),
    include(number, Dmgs, Damaged).

parse(Str, Rows) :- string_lines(Str, Lines),
                    maplist(parse_row, Lines, Rows).
in(Rows) :- read_file_to_string('day12.txt', Str, []), parse(Str, Rows).

damaged(35). % #
working(46). % .
unknown(63). % ?

damaged_count(35-C, C).
damaged_counts(In, Damaged) :- clumped(In, Clumps), convlist(damaged_count, Clumps, Damaged).

% Possibly naive solution, try to replace ? with # or . and check
guess([],[]).
guess([63|RestIn], [35|RestOut]) :- guess(RestIn, RestOut).
guess([63|RestIn], [46|RestOut]) :- guess(RestIn, RestOut).
guess([C|RestIn], [C|RestOut]) :- \+ unknown(C), guess(RestIn, RestOut).

possible_configuration(Line, Damaged, Config) :-
    guess(Line, Config), damaged_counts(Config, Damaged).

count_possible_configurations(Line-Damaged, Count) :-
    aggregate_all(count, possible_configuration(Line, Damaged, _), Count).

part1(Total) :-
    in(Rows),
    maplist(count_possible_configurations2, Rows, C), sum_list(C, Total).

% part1(8193).


%% Part2 5x the line (separated by ?) and 5x the counts

times5(LineIn-DmgIn, LineOut-DmgOut) :-
     util:repeat(5, `?`, LineIn, LineOut),
     util:repeat(5, DmgIn, DmgOut).

damaged_run([], 1, []). % damaged run at end of line
damaged_run([C|Line], 1, Line) :- \+ damaged(C). % must have atleast ? or . between runs

damaged_run([C|Line], D, Rest) :- D > 1, \+ working(C),
                                  D1 is D - 1,
                                  damaged_run(Line, D1, Rest).

configs([], [], 1). % No more line or contiguous, this solution works!
configs([], [_|_], 0). % some dmgs unused
configs([35|_], [], 0). % still damaged, but groups used
configs([35|Line], [D|_], 0) :- \+ damaged_run(Line, D, _).
configs([35|Line], [D|Dmgs], Solutions) :- % # must be part of damaged
    damaged_run(Line, D, RestOfLine),
    configs(RestOfLine, Dmgs, Solutions).
configs([63|Line], Dmgs, Solutions) :- % ?
    configs([35|Line], Dmgs, S1), % ? might be a # (35)
    configs(Line, Dmgs, S2),      % ? might be a ., recurse
    Solutions is S1 + S2.
configs([46|Line], Dmgs, Solutions) :- % .
    configs(Line, Dmgs, Solutions).

count_possible_configurations2(Line-Dmgs, Solutions) :-
    configs(Line, Dmgs, Solutions).

part2(Total) :-
    in(Rows),
    foldl([Row,Sum0,Sum1]>>(times5(Row,Row5x),
                            count_possible_configurations2(Row5x, C),
                            Sum1 is Sum0 + C), Rows, 0, Total).

% part2(45322533163795).
