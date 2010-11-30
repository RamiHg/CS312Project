:- dynamic active_player/1.
:- dynamic current_player/1.
:- dynamic controlling_player/1.
:- dynamic room/1.
:- dynamic weapon/1.

:- dynamic last_play_id/1.

:- dynamic recorded_suggestion/3.
:- dynamic recorded_to_player_response/3.
:- dynamic recorded_to_opponent_response/2.

/*
    Game Setup
*/

init_clue(Type, Players, ControllingPlayer) :-
    % Wipe everything first
    wipe,
	init_weapons(Type),
	init_rooms(Type),
	init_players(Players),
	init_first_player,
    assert(controlling_player(ControllingPlayer)),
    assert(last_play_id(0)).

init_players([]).
init_players([Name|Names]) :-
    assert(active_player(Name)),
    init_players(Names).

% If mrsPeacock is playing, then she's first
init_first_player :-
    active_player(mrsPeacock),
    assert(current_player(mrsPeacock)).
% Else, find whoever is to the left
init_first_player :-
    assert(current_player(mrsPeacock)),
    next_turn(TrueFirst),
    set_current_player(TrueFirst).
    
% wipe game databases
wipe :-
	retractall(weapon(_)),
	retractall(room(_)),
	retractall(active_player(_)),
	retractall(current_player(_)),
    retractall(controlling_player(_)),
    retractall(recorded_suggestion(_,_,_)),
    retractall(recorded_to_player_response(_,_,_)),
    retractall(recorded_to_opponent_response(_,_)).
    
/*
    Game Cycles
*/

set_current_player(Player) :-
    retractall(current_player(_)),
    assert(current_player(Player)).

next_turn(NextPlayer) :-
    current_player(Current),
    relatively_left(Current, NextPlayer),
    % That's active
    active_player(NextPlayer),
    % No more
    !.
    
/* 
    Main Gameplay
*/

new_suggestion(Id, Player, suggestion(Location, Suspect, Weapon)) :-
    % Get a new ID for the suggestion
    new_play_id(Id),
    % Record it
    assert(recorded_suggestion(Id, Player, suggestion(Location, Suspect, Weapon))).
    
% This happens when someone responds to a move the controlling player makes
player_suggestion_response(SuggestionId, Opponent, DebunkingCard) :-
    % We know that the opponent posesses this card
    assert(card_in_possession(Opponent, DebunkingCard)),
    % Record this response
    assert(recorded_to_player_response(SuggestionId, Opponent, DebunkingCard)).
    
% This happens when someone responds to a move an oppponent makes
opponent_suggestion_response(SuggestionId, Debunker) :-
    % TODO: Mark all cards this his suggestion as possibly in the hand of someone
    assert(recorded_to_opponent_response(SuggestionId, Debunker)).
    
new_play_id(X) :-
    last_play_id(LastId),
    X is LastId + 1,
    retract(last_play_id(_)),
    assert(last_play_id(X)).

/*
    User Interface
*/

main :-
    write('Clue Assistant 1.0\n'),
    ask_school(School),
    ask_players(Players),
    ask_controller(ControllingPlayer),
    init_clue(School, Players, ControllingPlayer),
    write('Game Initialized.\n').
    
ask_school(School) :-
    write('School of Thought [oldSchool, newSchool]: '),
    read(School).
    
ask_players(Players) :-
    write('Type the participating players in the following format\n'),
    write('  [player0, player1, etc..]: '),
    read(Players).
    
ask_controller(ControllingPlayer) :-
    write('Who are you playing as?: '),
    read(ControllingPlayer).
    
main_menu(0) :-
    print_main_menu,
    read(Option),
    main_menu(Option).
    
main_menu(1) :-
    new_move(0).

print_main_menu :-
    write('[1] Record a new suggestion\n').
    
new_move(0) :-
    print_new_move,
    read(Option),
    new_move(Option).
    
print_new_move :-
    write('[1] Record a suggestion you made\n'),
    write('[2] Record a suggestion an opponent made\n').
    

/*
    Game Elements and Rules
*/

character(missScarlet).
character(colonelMustard).
character(mrsWhite).
character(mrGreen).
character(mrsPeacock).
character(professorPlum).

% Player Spatial Ordering
left_of(missScarlet, colonelMustard).
left_of(colonelMustard, mrsWhite).
left_of(mrsWhite, mrGreen).
left_of(mrGreen, mrsPeacock).
left_of(mrsPeacock, professorPlum).
left_of(professorPlum, missScarlet).

relatively_left(LeftPlayer, RightPlayer) :- left_of(LeftPlayer, RightPlayer).
relatively_left(LeftPlayer, RightPlayer) :-
    left_of(LeftPlayer, DirectlyLeft),
    relatively_left(DirectlyLeft, RightPlayer).

init_weapons(oldSchool) :-
    assert(weapon(knife)),
    assert(weapon(candlestick)),
    assert(weapon(revolver)),
    assert(weapon(rope)),
    assert(weapon(pipe)),
    assert(weapon(wrench)).
    
init_weapons(newSchool) :-
    assert(weapon(knife)),
    assert(weapon(candlestick)),
    assert(weapon(pistol)),
    assert(weapon(rope)),
    assert(weapon(bat)),
    assert(weapon(ax)).
    
init_rooms(oldSchool) :-
    assert(room(kitchen)),
    assert(room(ballroom)),
    assert(room(conservatory)),
    assert(room(billiard)),
    assert(room(library)),
    assert(room(study)),
    assert(room(hall)),
    assert(room(lounge)),
    assert(room(dining_room)).
    
init_rooms(newSchool) :-
    assert(room(kitchen)),
    assert(room(patio)),
    assert(room(spa)),
    assert(room(theatre)),
    assert(room(living)),
    assert(room(observatory)),
    assert(room(hall)),
    assert(room(guest)),
    assert(room(dining)).