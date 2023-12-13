:- use_module(library(dcg/basics)).
dir(right) --> [82].
dir(left) --> [76].
dirs([D|Ds]) --> dir(D), dirs(Ds).
dirs([]) --> eol.

nodename(N) --> string_without(` ,)`, Cs), { atom_codes(N, Cs) }.
node(node(Node, Left, Right)) --> nodename(Node), ` = (`, nodename(Left), `, `, nodename(Right), `)`.

nodes([]) --> eos.
nodes([N|Ns]) --> node(N), eol, nodes(Ns).
input(Ds, Ns) --> dirs(Ds), eol, nodes(Ns).

cycle([I|Items], I, NextItems) :- append(Items, [I], NextItems).

node_by_name(Nodes, Name, node(Name,L,R)) :- member(node(Name, L, R), Nodes).
node_dir(node(_, L, _), left, L).
node_dir(node(_, _, R), right, R).

travel(Nodes, D, At, NewAt) :- node_dir(At, D, NextNodeName),
                               node_by_name(Nodes, NextNodeName, NewAt).

travel(_, _, node('ZZZ',_,_), S, S). % At ZZZ node, stop traveling
travel(Dirs, Nodes, At, StepsTaken, TotalSteps) :-
    cycle(Dirs, D, Dirs1),
    travel(Nodes, D, At, NextNode),
    StepsTaken1 is StepsTaken + 1, !,
    travel(Dirs1, Nodes, NextNode, StepsTaken1, TotalSteps).

part1(Ans) :-
    phrase_from_file(input(Ds,Ns), 'day8.txt'),
    node_by_name(Ns, 'AAA', A),
    travel(Ds,Ns,A,0,Ans).
% part1(16343).

% Part2 start on every node that starts with A and step them all until
% every At node starts with Z

a_node(node(Name,_,_)) :- atom_codes(Name, Cs), last(Cs, 65).
z_node(node(Name,_,_)) :- atom_codes(Name, Cs), last(Cs, 90).

steps_to_z(_, _, Z, S) --> { z_node(Z) }, [S].
steps_to_z(Nodes, Dirs, At, Steps) --> { cycle(Dirs, D, Dirs1),
                                         travel(Nodes, D, At, NewAt),
                                         Steps1 is Steps + 1 },
                                       steps_to_z(Nodes, Dirs1, NewAt, Steps1).

part2(Ans) :-
    phrase_from_file(input(Ds,Ns), 'day8.txt'),
    convlist([N,N]>>a_node(N), Ns, ANodes),
    maplist({Ds,Ns}/[N,S]>>phrase(steps_to_z(Ns,Ds,N,0),[S]), ANodes, Steps),
    foldl([S1,S2,Lcm]>>(Lcm is lcm(S1,S2)), Steps, 1, Ans).

% part2(15299095336639).
