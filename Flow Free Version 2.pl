% The test case in the assignment file
grid(4,3).
dot('G',[3,1]).
dot('G',[2,2]).
%-----------------------------------------------------------------------------------------------------
% Main Predicate (Solve)
solve():-
       findall(List , dot(List , _) , NewList),
       sort(NewList,[H|T]),
       solveTwo([H|T]).
%-----------------------------------------------------------------------------------------------------
% SolveTwo Predicate
solveTwo([]):-!.
solveTwo([H|T]):-
       shortestPath(H),
       solveTwo(T).
%-----------------------------------------------------------------------------------------------------
% ShortestPath Predicate
shortestPath(X):-
	findall(ListOne , dot(X , ListOne) , List),
	nth0(0 ,  List , Start),
	nth0(1 , List , GoalState),
        getHeuristic(Start, H, GoalState),
        path([[Start,null, 0, H, H]],[],GoalState).
%-----------------------------------------------------------------------------------------------------
% getHeuristic Predicate
getHeuristic([], 0, []):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).
getHeuristic([_|T1],H,[_|T2]):-
	getHeuristic(T1,TH, T2),
	H is TH + 1.
%-----------------------------------------------------------------------------------------------------
% Moves Predicates
moves( State, Opened, Closed,[Next,State, NPC, H, TC], PC, GoalState):-
		move(State,Next),
		\+ member([Next, _, _, _, _],Opened),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
		getHeuristic(Next, H, GoalState),
		TC is NPC + H.
move(S , NextState):-
  	moveright(S,NextState).
% Right Moves
moveright([R1 , R2],[R1 , NewR2]):-
  	NewR2 is R2 + 1.
move(S,NextState):-
  	moveleft(S,NextState).
% Left Moves
moveleft([R1  , R2],[R1 , NewR2]):-
  	NewR2 is R2 - 1.
move(S,NextState):-
  	movedown(S,NextState).
% Down Moves
movedown([R1 , R2],[NewR1 , R2]):-
  	NewR1 is R1 + 1.
move(S,NextState):-
  	moveup(S,NextState).
% Up Moves
moveup([R1 , R2],[NewR1 , R2]):-
  	NewR1 is R1 - 1.
%-----------------------------------------------------------------------------------------------------
% Path Predicate
path([], _, _):-
		write('No solution'),nl,!.
path(Opened, Closed, GoalState):-
		bestChild(Opened, [GoalState, Parent, PC, H, TC], RestOfOpen),
		write('A solution is found'),  nl ,
		printsolution([GoalState,Parent, PC, H, TC], Closed),!.
path(Opened, Closed, GoalState):-
		bestChild(Opened, [State, Parent, PC, H, TC], RestOfOpen),
		children(State, Opened, Closed, Children, PC, GoalState),
		addList(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent, PC, H, TC] | Closed], GoalState).
%-----------------------------------------------------------------------------------------------------
% Add and remove from lists Predicates
addList(Children, [], Children).
addList(Children, [H|Open], [H|NewOpen]):-
		addList(Children, Open, NewOpen).
removeList(_, [], []):-!.
removeList(H, [H|T], V):-
	!, removeList(H, T, V).
removeList(H, [H1|T], [H1|T1]):-
	removeList(H, T, T1).
%-----------------------------------------------------------------------------------------------------
% Get Children and Best Children Predicates
children(State, Open ,Closed , Children, PC, Goal):-
		bagof(X, moves( State, Open, Closed, X, PC, Goal), Children) .
children(_,_,_, [],_,_).
bestChild([Child], Child, []).
bestChild(Open, Best, RestOpen):-
	bestChildTwo(Open, Best),
	removeList(Best, Open, RestOpen).
bestChildTwo([State], State):-!.
bestChildTwo([State|Rest], Best):-
	bestChildTwo(Rest, Temp),
	getBest(State, Temp, Best).
getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-
	TC < TC1, !.
getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).
%-----------------------------------------------------------------------------------------------------
% To Print Solution
printsolution([State, null, PC, H, TC],_):-!,
		write(State), write(' PC-> '), write(PC), write(' H->'), write(H), write(' TC-> '), write(TC), nl.
printsolution([State, Parent, PC, H, TC], Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),
		write(State), write(' PC-> '), write(PC), write(' H->'), write(H), write(' TC-> '), write(TC), nl.