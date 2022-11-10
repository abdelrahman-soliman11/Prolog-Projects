% "-1" Represent the blank cell.
% Eight Cells from "0" to "7".
% Each cell can slide (up , down , right , left) into the blank cell.
% The goal is rearranging celles in form [-1 , 0 , 1 , 2 , 3 , 4 , 5 , 6 , 7].

%moves in top row (We can move down , right , or left have two prospects to move right or move left and three prospects to move down).

%Top Row

%move Left
move([-1,R2,R3,R4,R5,R6,R7,R8,R9] , [R2,-1,R3,R4,R5,R6,R7,R8,R9]).
move([R1,-1,R3,R4,R5,R6,R7,R8,R9] , [R1,R3,-1,R4,R5,R6,R7,R8,R9]).

%move Right
move([R1,-1,R3,R4,R5,R6,R7,R8,R9] , [-1,R1,R3,R4,R5,R6,R7,R8,R9]).
move([R1,R2,-1,R4,R5,R6,R7,R8,R9] , [R1,-1,R2,R4,R5,R6,R7,R8,R9]).

%move Down
move([R1,R2,R3,-1,R5,R6,R7,R8,R9] , [-1,R2,R3,R1,R5,R6,R7,R8,R9]).
move([R1,R2,R3,R4,-1,R6,R7,R8,R9] , [R1,-1,R3,R4,R2,R6,R7,R8,R9]).
move([R1,R2,R3,R4,R5,-1,R7,R8,R9] , [R1,R2,-1,R4,R5,R3,R7,R8,R9]).

%moves in middle row (We can move down , right , up ,  or left , we have two prospects to move right or move left and three prospects to move down and move up).

%Middle Row

%move Left
move([R1,R2,R3,-1,R5,R6,R7,R8,R9] , [R1,R2,R3,R5,-1,R6,R7,R8,R9]).
move([R1,R2,R3,R4,-1,R6,R7,R8,R9] , [R1,R2,R3,R4,R6,-1,R7,R8,R9]).

%move Right
move([R1,R2,R3,R4,-1,R6,R7,R8,R9] , [R1,R2,R3,-1,R4,R6,R7,R8,R9]).
move([R1,R2,R3,R4,R5,-1,R7,R8,R9] , [R1,R2,R3,R4,-1,R5,R7,R8,R9]).

%move Up
move([-1,R2,R3,R4,R5,R6,R7,R8,R9] , [R4,R2,R3,-1,R5,R6,R7,R8,R9]).
move([R1,-1,R3,R4,R5,R6,R7,R8,R9] , [R1,R5,R3,R4,-1,R6,R7,R8,R9]).
move([R1,R2,-1,R4,R5,R6,R7,R8,R9] , [R1,R2,R6,R4,R5,-1,R7,R8,R9]).

%move Down
move([R1,R2,R3,R4,R5,R6,-1,R8,R9] , [R1,R2,R3,-1,R5,R6,R4,R8,R9]).
move([R1,R2,R3,R4,R5,R6,R7,-1,R9] , [R1,R2,R3,R4,-1,R6,R7,R5,R9]).
move([R1,R2,R3,R4,R5,R6,R7,R8,-1] , [R1,R2,R3,R4,R5,-1,R7,R8,R6]).


%moves in last row (We can move right , up ,  or left , we have two prospects to move right or move left and three prospects to move up).

% Last Row
%move Left
move([R1,R2,R3,R4,R5,R6,-1,R8,R9] , [R1,R2,R3,R4,R5,R6,R8,-1,R9]).
move([R1,R2,R3,R4,R5,R6,R7,-1,R9] , [R1,R2,R3,R4,R5,R6,R7,R9,-1]).

%move Right
move([R1,R2,R3,R4,R5,R6,R7,-1,R9] , [R1,R2,R3,R4,R5,R6,-1,R7,R9]).
move([R1,R2,R3,R4,R5,R6,R7,R8,-1] , [R1,R2,R3,R4,R5,R6,R7,-1,R8]).

%move Up
move([R1,R2,R3,-1,R5,R6,R7,R8,R9] , [R1,R2,R3,R7,R5,R6,-1,R8,R9]).
move([R1,R2,R3,R4,-1,R6,R7,R8,R9] , [R1,R2,R3,R4,R8,R6,R7,-1,R9]).
move([R1,R2,R3,R4,R5,-1,R7,R8,R9] , [R1,R2,R3,R4,R5,R9,R7,R8,-1]).

% General Algorithm
go([-1,0,1,2,3,4,5,6,7],Visited,Visited).
puzzle(StartState,Visited,R):-
move(StartState,Next),
\+ member(Next,Visited),
write(Next),nl,
go(Next,[Next|Visited],R).
play(S,G):-
go(S,[S],G),!.
