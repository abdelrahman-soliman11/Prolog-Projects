% Test Case in the assignment
grid(4,3).
dot('R',[3,0]).
dot('R',[0,2]).
dot('B',[2,1]).
dot('B',[1,2]).
dot('G',[3,1]).
dot('G',[2,2]).
%-----------------------------------------------------------------------------------------------------
% Moves Predicates
moveright([State,Index],Color,[NewState,NewIndex]):-
  	grid(_,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    C < Cols - 1,
    NewCol is C + 1,
    NewIndex is R * Cols + NewCol,
  	substitute(State,NewIndex,Color,NewState).
moveleft([State,Index],Color,[NewState,NewIndex]):-
  	grid(_,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    C > 0,
    NewCol is C - 1,
    NewIndex is R * Cols + NewCol,
  	substitute(State,NewIndex,Color,NewState).
moveup([State,Index],Color,[NewState,NewIndex]):-
  	grid(_,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    R > 0,
    NewR is R - 1,
    NewIndex is NewR * Cols + C,
  	substitute(State,NewIndex,Color,NewState).
movedown([State,Index],Color,[NewState,NewIndex]):-
  	grid(Rows,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    R < Rows - 1,
    NewR is R + 1,
    NewIndex is NewR * Cols + C,
  	substitute(State,NewIndex,Color,NewState).
move(S,Color,Snew):-
  	moveright(S,Color,Snew);
    movedown(S,Color,Snew);
    moveleft(S,Color,Snew);
  	moveup(S,Color,Snew).
moves( State, Color,O, C,Next):-
    move(State,Color,Next),
    \+ member(Next,O),
    \+ member(Next,C).
%-----------------------------------------------------------------------------------------------------
substitute(List,Index,Value,NewList):-
    substitute(List,0,Index,Value,NewList).
substitute([H|T],Index,Index,Value,[Value|T]):- H = '_'; H = Value,!.
substitute([H|T],I,Index,Value,[H|NewList]):-
    NewI is I + 1,
    substitute(T,NewI,Index,Value,NewList).
%-----------------------------------------------------------------------------------------------------
% Init Predicate
init(Index, []):-
    grid(Rows,Cols),
    R is Index // Cols,
    R >= Rows, !.
init(Index, [Color|Start]):-
    grid(_,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    dot(Color,[R,C]),
    NewIndex is Index + 1,
    init(NewIndex, Start), !.
init(Index, ['_'|Start]):-
    NewIndex is Index + 1,
    init(NewIndex, Start).
%-----------------------------------------------------------------------------------------------------
% Main Predicate
solve:-
    init(0,Start),
    setof(X, C^dot(X,C), Colors),
    solve(Start,Colors,Sol).
solve(_,[],[]):- !.
solve(Start,[Color|RestOfColors],[Goal|Sol]):-
    nth0(N,Start,Color),
    path([[Start,N]],[],Color,N,Goal),
    solve(Goal,RestOfColors,Sol).
%-----------------------------------------------------------------------------------------------------
% Path Predicate
path([],_,_,_,_):- fail.
path([[Goal,Index] | _], _, Color,Initial, Goal):-
    \+ Index is Initial,
    grid(_,Cols),
    R is Index // Cols,
    C is Index mod Cols,
    dot(Color,[R,C]).
path(O, C, Color,Initial, Goal):-
    deleteList(O, State, RestOfOpen),
    getchildren(State, Color,O, C, Children),
    addList(Children , RestOfOpen, NewOpen),
    path(NewOpen, [State | C], Color,Initial, Goal).
%-----------------------------------------------------------------------------------------------------
% Get Children Predicate
getchildren(State, Color, O ,C , Children):-
    findall(X, moves( State, Color,O, C, X), Children), ! .
%-----------------------------------------------------------------------------------------------------
% Add and delete predicates from list
addList(Children,[],Children).
addList(L, [H|O], [H|NewOpen]):-
    addList(L, O, NewOpen).
deleteList([State|RestOpen], State, RestOpen).
%-----------------------------------------------------------------------------------------------------
