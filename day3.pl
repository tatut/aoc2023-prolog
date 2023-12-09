digit(C) :- C >= 48, C =< 57.

% Anything that is not a dot or a digit is a symbol
symbol(C) :- \+ C = 46, \+ digit(C).

% read file into g(Width,Height,Lines) term that can be easily
% accessed.

in(g(Width,Height,Lines)) :-
    read_file_to_string('day3.txt', Str, []),
    split_string(Str, "\n", "", AllLines),
    exclude([L]>>string_length(L,0), AllLines, Lines),
    [L|_] = Lines,
    string_length(L, Width),
    length(Lines, Height).

% Access grid at X,Y (counting starts at 1)
at(g(_,_,Lines), X, Y, C) :-
    nth1(Y, Lines, Line),
    string_code(X, Line, C), !.

% Anything outside range is considered a dot
at(g(W,H,_), X, Y, 46) :- (X < 1; X > W; Y < 1; Y > H), !.

at_x(G, Y, X, C) :- at(G, X, Y, C).

% A number starts at a position that has a digit
% and a non-digit on its left side.
% Number ends at a digit that has a non-digit on its
% right side.
%
% succeeds with: num(Number, Xstart-Xend, Y)
numbers(G, num(Number, X-X1, Y)) :-
    G=g(W,H,_),
    between(1, H, Y), between(1, W, X),
    at(G, X, Y, C),
    digit(C),
    Left is X - 1,
    at(G, Left, Y, LCs),
    \+ digit(LCs),
    number_end(G, X, Y, X1),
    number_value(G, X-X1, Y, Number).

is_digit_x(G, Y, X) :- at(G, X, Y, Cd), digit(Cd).

number_end(G, X, Y, X1) :-
    between(X, inf, X1),
    numlist(X,X1,Xs),
    maplist(is_digit_x(G,Y), Xs),
    Right is X1 + 1,
    at(G, Right, Y, RCs),
    \+ digit(RCs), !.

number_value(G, Xs-Xe, Y, N) :-
    numlist(Xs, Xe, Xpos),
    maplist(at_x(G,Y), Xpos, Cs),
    number_codes(N, Cs).

% Check if there is a symbol adjacent to any position
part_number(G, num(_, Xs-Xe, Y)) :-
    between(Xs, Xe, X),
    neighbor(X, Y, Xp-Yp),
    at(G, Xp, Yp, C),
    symbol(C), !.

neighbor(X, Y, Xp-Yp) :-
    X1 is X - 1, X2 is X + 1,
    Y1 is Y - 1, Y2 is Y + 1,
    between(Y1, Y2, Yp),
    between(X1, X2, Xp),
    \+ ( Xp = X, Yp = Y).

part1(Ans) :-
    in(G),
    aggregate_all(sum(Num), (numbers(G, N), part_number(G, N),
                             N = num(Num, _, _)), Ans).

% part1(535351).

%% Part 2

adjacent_to_gear(G, num(_, Xs-Xe, Y), Xg-Yg) :-
    between(Xs, Xe, X),
    neighbor(X,Y,Xg-Yg),
    at(G, Xg, Yg, 42).

% It is much faster to first call findall on numbers
% instead of doing numbers call on each N1 and N2
gears(G, Ns) :-
    findall(Num, numbers(G, Num), Nums),
    member(N1, Nums),
    adjacent_to_gear(G, N1, Gear),
    member(N2, Nums),
    \+ N1 = N2,
    adjacent_to_gear(G, N2, Gear),
    sort([N1,N2], Ns).

part2(Ans) :-
    in(G),
    setof(Ns, gears(G, Ns), Gears),
    maplist([[num(G1,_,_),num(G2,_,_)],Product]>>(Product is G1 * G2), Gears, Products),
    sum_list(Products, Ans).

% part2(87287096).
