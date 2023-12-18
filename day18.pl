:- use_module(library(dcg/basics)).

in(Plans) :- phrase_from_file(plans(Plans), 'day18.txt', []).

plans([]) --> eos.
plans([p(Dir,C,Col)|Plans]) -->
    [D], { atom_codes(Dir0, [D]), downcase_atom(Dir0, Dir) },
    ` `, integer(C),
    ` (#`, string_without(`)`,Col), `)`, eol,
    plans(Plans).

next(p(u,C,_), X1-Y1, X1-Y2) :- Y2 is Y1 - C.
next(p(d,C,_), X1-Y1, X1-Y2) :- Y2 is Y1 + C.
next(p(l,C,_), X1-Y1, X2-Y1) :- X2 is X1 - C.
next(p(r,C,_), X1-Y1, X2-Y1) :- X2 is X1 + C.

line(_, [], []).
line(At, [P|Plans], [X-Y|Rest]) :- next(P, At, X-Y),
                                   line(X-Y, Plans, Rest).

% Convert plans into a polygon so we can calculate its area
plans_polygon(Plans, [1-1|Polygon]) :-
    line(1-1, Plans, Polygon).

dist(X1-Y1, X2-Y2, Dist) :- Dist is abs(X2-X1) + abs(Y2-Y1).

line_length([_], 0).
line_length([From,To|Rest], Len) :-
    line_length([To|Rest], Len0),
    dist(From,To, Len1),
    Len is Len0 + Len1.



% polygon area
area([_], Acc, Area) :- Area is Acc / 2.
area([X1-Y1,X2-Y2|Points], Acc0, Area) :-
    Acc is Acc0 + ((X1 * Y2) - (X2 * Y1)),
    area([X2-Y2|Points], Acc, Area).

area(Poly, Area) :- area(Poly, 0, Area).

filled(Ps, Filled) :-
    plans_polygon(Ps, Poly),
    area(Poly, A),
    line_length(Poly, L),
    Filled is A + L/2 + 1.

part1(Ans) :- in(Ps), filled(Ps, Ans).
% part1(33491).

hex_dir(48, r).
hex_dir(49, d).
hex_dir(50, l).
hex_dir(51, u).

fix_instruction(p(_,_,Col), p(Dir,Count,_)) :-
    length(CountCs, 5),
    append(CountCs, [DirC], Col),
    phrase(xinteger(Count), CountCs),
    hex_dir(DirC, Dir).


part2(Ans) :-
    in(Ps0),
    maplist(fix_instruction, Ps0, Ps1),
    filled(Ps1, Ans).
% part2(87716969654406).
