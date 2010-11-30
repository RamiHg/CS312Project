:- dynamic seen/1.
:- dynamic active_player/1.
:- dynamic current_player/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic seen_suggestion/2.

%initialize characters
%these never change
character(scarlet).
character(mustard).
character(white).
character(green).
character(peacock).
character(plum).

%spatial relations between characters
left_of(scarlet, mustard).
left_of(mustard, white).
left_of(white, green).
left_of(green, peacock).
left_of(peacock, plum).
left_of(plum, scarlet).

%meta left_of relation
relatively_left(Left, Right) :-
	left_of(Left, Right).
relatively_left(Left, Right) :-
	left_of(Left, Intermediate),
	relatively_left(Intermediate, Right).

%initialize weapons
%0 for old school, 1 for new school
initWeapons(0) :-
	assert(weapon(knife)),
	assert(weapon(candlestick)),
	assert(weapon(revolver)),
	assert(weapon(rope)),
	assert(weapon(pipe)),
	assert(weapon(wrench)).

initWeapons(1) :-
	assert(weapon(knife)),
	assert(weapon(candlestick)),
	assert(weapon(pistol)),
	assert(weapon(rope)),
	assert(weapon(bat)),
	assert(weapon(ax)).

%initialize rooms
%0 for old school, 1 for new school
initRooms(0) :-
	assert(room(kitchen)),
	assert(room(ballroom)),
	assert(room(conservatory)),
	assert(room(billiard)),
	assert(room(library)),
	assert(room(study)),
	assert(room(hall)),
	assert(room(lounge)),
	assert(room(dining)).

initRooms(1) :-
	assert(room(kitchen)),
	assert(room(patio)),
	assert(room(spa)),
	assert(room(theatre)),
	assert(room(living)),
	assert(room(observatory)),
	assert(room(hall)),
	assert(room(guest)),
	assert(room(dining)).

setCurrentPlayer(Player) :-
	retract(current_player(_)),
	assert(current_player(Player)).

initPlayers([]).
initPlayers([Name|Names]) :-
	assert(active_player(Name)),
	initPlayers(Names).

nextTurn(Next) :-
	current_player(Current),
	relatively_left(Current, Next),
	active_player(Next),
	!.

initClue(Type, Players) :-
	initWeapons(Type),
	initRooms(Type),
	initPlayers(Players),
	assert(current_player(plum)),
 	%first player is always scarlet, so we seek first active player after plum
	nextTurn(Next),
	setCurrentPlayer(Next).	
	
%wipe game databases
wipe :-
	retract(weapon(_)),
	retract(room(_)),
	retract(active_player(_)),
	retract(current_player(_)).

%when players gets shown a card/cards
shown([]).
shown([Card|Cards]) :-
	assert(seen(Card)), shown(Cards).

%get_ functions for fetching game data
%to be used for menus etc.
%get play defaults
getCharacters(Chars) :-
	setof(X, character(X), Chars).

getWeapons(Weapons) :-
	setof(X, weapon(X), Weapons).

getRooms(Rooms) :-
	setof(X, room(X), Rooms).

getAll(All) :-
	getCharacters(C),
	getWeapons(W),
	getRooms(R),
	append(W,R,Part),
	append(C,Part,All).

%get eliminated cards
getEliminated(All) :-
	setof(X, seen(X), All).

getEliminatedCharacters(Chars) :-
	setof(X, (seen(X),character(X)), Chars).

getEliminatedWeapons(Weapons) :-
	setof(X, (seen(X),weapon(X)), Weapons).

getEliminatedRooms(Rooms) :-
	setof(X, (seen(X),room(X)), Rooms).

%get remaining/unknown cards
getRemainingCharacters(Chars) :-
	getCharacters(C),
 	getEliminatedCharacters(E),
 	subtract(C,E,Chars).

getRemainingWeapons(Weapons) :-
	getWeapons(W),
	getEliminatedWeapons(E),
	subtract(W,E,Weapons).

getRemainingRooms(Rooms) :-
	getRooms(R),
	getEliminatedRooms(E),
	subtract(R,E,Rooms).

getRemainingAll(All) :-
	getAll(A),
	getEliminated(E),
	subtract(A,E,All).

%print the contents of any list
printList([]).
printList([H|T]) :-
	print(H),
	print(', '),
	printList(T).

%check for/get accusation
checkAccusation :-
	getRemainingCharacters(RC),
	getRemainingWeapons(RW),
	getRemainingRooms(RR),
	length(RC,1),
	length(RW,1),
	length(RR,1),
	append(RW,RR,Part),
	append(RC,Part,Accusation),
	print('Accusation found:'),
	printList(Accusation).


