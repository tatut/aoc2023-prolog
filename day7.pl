:- module(day7).
:- use_module(library(dcg/basics)).
% represent hand as a list of cards

t(`32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483`). % test input

% Parsing
hand_bid(hand_bid(Hand, Bid)) --> string_without(` `, Hand), ` `, integer(Bid).
hand_bids([]) --> eol.
hand_bids([H|Hs]) --> hand_bid(H), eol, hand_bids(Hs).
in(H) :- phrase_from_file(hand_bids(H), 'day7.txt').

% Determine hand strengths
counts(List, Counts) :- msort(List, Items), clumped(Items, Counts).

% Five of a kind, where all five cards have the same label: AAAAA
hand_strength([A,A,A,A,A], 7).

% Four of a kind, where four cards have the same label and one card has a different label: AA8AA
hand_strength(Hand, 6) :- counts(Hand, Cs), member(_-4, Cs).

% Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
hand_strength(Hand, 5) :- counts(Hand, Cs), member(_-3, Cs), member(_-2, Cs).

% Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
hand_strength(Hand, 4) :- counts(Hand, Cs), member(A-3, Cs), length(Cs, 3).

% Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
hand_strength(Hand, 3) :- counts(Hand, Cs), member(A-2, Cs), member(B-2, Cs), \+ A = B.

% One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
hand_strength(Hand, 2) :- counts(Hand, Cs), length(Cs, 4).

% High card, where all cards' labels are distinct: 23456
hand_strength(Hand, 1) :- counts(Hand, Cs), length(Cs, 5).

card_value(65, 14). % A
card_value(75, 13). % K
card_value(81, 12). % Q
card_value(74, 11). % J
card_value(84, 10). % T
card_value(D, V) :- between(50, 57, D), V is D - 48.

% check if H1 beats H2

% If strength of H1 is greater than that of H2
beats(H1, H2) :- hand_strength(H1, H1s), hand_strength(H2, H2s), H1s > H2s.

% If strength is same, check first card that is not the same
beats(H1, H2) :- hand_strength(H1, S), hand_strength(H2, S),
                 stronger_card(H1,H2).

stronger_card([C1|_], [C2|_]) :- card_value(C1, C1V), card_value(C2, C2V), C1V > C2V.
stronger_card([C|H1], [C|H2]) :- stronger_card(H1, H2).

compare_hand(<, hand_bid(H1,_), hand_bid(H2,_)) :- beats(H2, H1).
compare_hand(>, hand_bid(H1,_), hand_bid(H2,_)) :- beats(H1, H2).
compare_hand(=, hand_bid(H,_), hand_bid(H,_)). % should not have same hand in input

strength_order(HandBids, Sorted) :- predsort(compare_hand, HandBids, Sorted).

enumerate(_,[],[]).
enumerate(I,[X|Xs],[I-X|Rest]) :- I1 is I + 1, enumerate(I1, Xs,Rest).

part1(Ans) :-
    in(H),
    strength_order(H, Sorted),
    enumerate(1, Sorted, Enumerated),
    maplist([R-hand_bid(_,Bid),W]>>(W is R*Bid), Enumerated, Wins),
    sum_list(Wins, Ans).
% part1(249483956).

% Part2 J is joker with value 1

card_value2(65, 14). % A
card_value2(75, 13). % K
card_value2(81, 12). % Q
card_value2(74, 1).  % Joker!
card_value2(84, 10). % T
card_value2(D, V) :- between(50, 57, D), V is D - 48.

replace(Old, _, In, In) :- \+ In = Old.
replace(Old, New, Old, New).

% Try replacing J with any other card
joker(Hand, Pretend) :-
    member(74, Hand), % contains at least one joker
    % pretend to be any other card (except other jokers)
    card_value2(NewCard, _), \+ NewCard = 74,
    maplist(replace(74, NewCard), Hand, Pretend).

hand_strength2(Hand, Str) :-
    findall(Pretend, joker(Hand, Pretend), Pretends),
    %writeln(strongers(Pretends)),
    aggregate_all(max(S), (member(H, [Hand|Pretends]), hand_strength(H, S)), Str).

% If strength of H1 is greater than that of H2
beats2(H1, H2) :- hand_strength2(H1, H1s), hand_strength2(H2, H2s), H1s > H2s.

% If strength is same, check first card that is not the same
beats2(H1, H2) :- hand_strength2(H1, S), hand_strength2(H2, S),
                  stronger_card2(H1,H2).

stronger_card2([C1|_], [C2|_]) :- card_value2(C1, C1V), card_value2(C2, C2V), C1V > C2V.
stronger_card2([C|H1], [C|H2]) :- stronger_card2(H1, H2).

compare_hand2(<, hand_bid(H1,_), hand_bid(H2,_)) :- beats2(H2, H1).
compare_hand2(>, hand_bid(H1,_), hand_bid(H2,_)) :- beats2(H1, H2).
compare_hand2(=, hand_bid(H,_), hand_bid(H,_)). % should not have same hand in input

strength_order2(HandBids, Sorted) :- predsort(compare_hand2, HandBids, Sorted).

part2(Ans) :-
    in(H),
    strength_order2(H, Sorted),
    enumerate(1, Sorted, Enumerated),
    maplist([R-hand_bid(_,Bid),W]>>(W is R*Bid), Enumerated, Wins),
    sum_list(Wins, Ans).
% part2(252137472).

:- begin_tests(day7).
:- use_module(day7).

test(part1) :- part1(249483956).
test(part2) :- part2(252137472).

:- end_tests(day7).
