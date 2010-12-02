:- dynamic active_player/1.
:- dynamic current_player/1.
:- dynamic controlling_player/1.
:- dynamic room/1.
:- dynamic weapon/1.

:- dynamic last_play_id/1.

:- dynamic card_in_possession/2.
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
    retractall(recorded_to_opponent_response(_,_)),
    retractall(card_in_possession(_,_)).
    
/*
    Deduction
*/

get_eliminated_cards(All) :-
    findall(X, card_in_possession(_,X), All).
    
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
    retractall(last_play_id(_)),
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
    write('Game Initialized.\n\n'),
    main_menu(0).
    
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
    
% Main Menu
    
main_menu(0) :-
    print_main_menu,
    read(Option),
    main_menu(Option).
    
% Record a new suggestion
main_menu(1) :-
    new_move(0),
    main_menu(0).

% Show suggestion history
main_menu(2) :-
    fact_history_menu,
    main_menu(0).
    
% Show card facts
main_menu(3) :-
    card_fact_menu,
    main_menu(0).
    
% Repeat the menu
main_menu(_) :- main_menu(0).

print_main_menu :-
    write('[1] Record a new suggestion\n'),
    write('[2] Show suggestion history\n'),
    write('[3] Show card facts\n').
    
/*
    UI: Recording moves
*/

new_move(0) :-
    print_new_move,
    read(Option),
    suggestion_menu(Option).
   
    
print_new_move :-
    write('[1] Record a suggestion you made\n'),
    write('[2] Record a suggestion an opponent made\n').
   
% A player suggestion   
suggestion_menu(1) :-
    controlling_player(ControllingPlayer),
    generic_suggestion_menu(SuggestionId, ControllingPlayer),
    % Check for a response
    write('Did anyone show you a card? [yes, no]: '),
    read(Response),
    player_suggestion_response_menu(Response, SuggestionId).
    
% An opponent suggestion
suggestion_menu(2) :-
    write('Who made this suggestion?: '),
    read(Opponent),
    generic_suggestion_menu(SuggestionId, Opponent),
    write('Did anyone respond to this suggestion? [yes, no]: '),
    read(Response),
    opponent_suggestion_response_menu(Response, SuggestionId).

generic_suggestion_menu(SuggestionId, Player) :-
    write('(Suggestion) Who: '),
    read(Character),
    write('(Suggestion) Where: '),
    read(Where),
    write('(Suggestion) With What: '),
    read(Weapon),
    % Record this suggestion
    new_suggestion(SuggestionId, Player, suggestion(Where, Character, Weapon)).
    
% When someone responds to a suggestion a player made
player_suggestion_response_menu(no, _).
player_suggestion_response_menu(yes, SuggestionId) :-
    write('Who responded?: '),
    read(Debunker),
    write('Which card was shown?: '),
    read(Card),
    player_suggestion_response(SuggestionId, Debunker, Card).
    
% When someone responds to a suggestion an opponent made
opponent_suggestion_response_menu(no, _).
opponent_suggestion_response_menu(yes, SuggestionId) :-
    write('Who responded?: '),
    read(Debunker),
    opponent_suggestion_response(SuggestionId, Debunker).
    
    
/*
    UI: Move history
*/
fact_history_menu :-
    % For each suggestion,
    findall(Id,recorded_suggestion(Id,_,_),L),
    print_facts(L).
    
print_facts([]).
print_facts([SuggestionId|Ids]) :-
    print_suggestion(SuggestionId),
    print_suggestion_response(SuggestionId),
    write('\n'),
    print_facts(Ids).
      
print_suggestion(SuggestionId) :-
    recorded_suggestion(SuggestionId, Player, suggestion(Location, Suspect, Weapon)),
    write(Player),
    write(' suggested ('),
    write(Suspect),
    write(','),
    write(Weapon),
    write(','),
    write(Location),
    write('). ').
    
print_suggestion_response(SuggestionId) :-
    % Is this a player suggestion?
    recorded_to_player_response(SuggestionId, Opponent, DebunkingCard),
    % It can't be anything else
    !,
    write(Opponent),
    write(' showed '),
    write(DebunkingCard).
    
print_suggestion_response(SuggestionId) :-
    % Is this an opponent suggestion?
    recorded_to_opponent_response(SuggestionId, Debunker),
    !,
    write(Debunker),
    write(' showed a card.').

print_suggestion_response(_) :-
    % Previous goals could not be proven
    write('No one responded.').
    
/*
    UI: Cards
*/
card_fact_menu :-
    findall(X, card_in_possession(_, X), L),
    print_card_facts(L).
    
print_card_facts([]).
print_card_facts([Card|Cards]) :-
    card_in_possession(Owner, Card),
    write(Card),
    write(' is owned by '),
    write(Owner),
    write('\n'),
    print_card_facts(Cards).

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