:- use_module(library(dcg/basics)).

% Parsing
card(card(Num, Winning, Have)) -->
    `Card`, whites, integer(Num), `: `, nums(Winning), `|`, nums(Have), eol.
nums([]) --> [].
nums([N|Ns]) --> whites, integer(N), whites, nums(Ns).
cards([]) --> eos.
cards([C|Cards]) --> card(C), cards(Cards).

in(Cards) :- phrase_from_file(cards(Cards), 'day4.txt').

%% Part 1

score(card(C, Winning, Have), Score) :-
    aggregate_all(count, (member(N, Have), member(N, Winning)), Matches),
    (Matches = 0 -> Score = 0;
     (Pow is Matches - 1,
      Score is 2 ^ Pow)).

part1(Ans) :-
    in(Cards),
    maplist(score, Cards, Scores),
    sum_list(Scores, Ans).
% part1(15205).

%% Part2 copy cards

card_n(AllCards, N, Card) :- member(Card, AllCards), Card=card(N,_,_).
cardnum(card(N,_,_), N).

copies(AllCards, C, Copies) :-
    card_n(AllCards, C, card(C, Winning, Have)),
    aggregate_all(count, (member(N, Have), member(N, Winning)), Matches),
    (Matches = 0 -> Copies=[];
     (NextCard is C + 1, LastCard is NextCard + Matches - 1,
      numlist(NextCard,LastCard, Ns),
      convlist(card_n(AllCards), Ns, CopyCards),
      maplist(cardnum,CopyCards,Copies))).

decrby(C, Count, AssocIn, AssocOut) :- del_assoc(C, AssocIn, Count, AssocOut).
decrby(C, Count, AssocIn, AssocOut) :-
    get_assoc(C, AssocIn, CountIn),
    CountIn > Count,
    CountOut is CountIn - Count,
    put_assoc(C, AssocIn, CountOut, AssocOut).

incrby(C, Count, AssocIn, AssocOut) :-
    \+ get_assoc(C, AssocIn, _),
    put_assoc(C, AssocIn, Count, AssocOut).

incrby(C, Count, AssocIn, AssocOut) :-
    get_assoc(C, AssocIn, CountIn),
    CountOut is CountIn + Count,
    put_assoc(C, AssocIn, CountOut, AssocOut).

add_cards([], Assoc, Assoc).
add_cards([card(C,_,_)|Cards], AssocIn, AssocOut) :-
    incrby(C, 1, AssocIn, Assoc1),
    add_cards(Cards, Assoc1, AssocOut).

play(AllCards, Total) :-
    % Create assoclist of cards to play and total counts
    % key is the card number and value is how many there are
    add_cards(AllCards, t, ToPlay),
    play(AllCards, ToPlay, ToPlay, Have),
    assoc_to_values(Have, Lst),
    sum_list(Lst, Total).

play(AllCards, t, Have, Have). % All cards played
play(AllCards, ToPlay, HaveIn, HaveOut) :-
    min_assoc(ToPlay, Card, Count),
    copies(AllCards, Card, Copies),
    foldl({Count}/[Copy,Have0,Have1]>>(incrby(Copy, Count, Have0, Have1)),
          Copies, HaveIn, HaveOut1),
    decrby(Card, Count, ToPlay, ToPlay1),
    foldl({Count}/[Copy,A0,A1]>>(incrby(Copy, Count, A0, A1)),
          Copies, ToPlay1, ToPlay2),
    play(AllCards, ToPlay2, HaveOut1, HaveOut).

part2(Ans) :-
    in(Cards),
    play(Cards, Ans).
% part2(6189740).
