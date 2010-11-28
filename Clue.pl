% The players that are currently playing
:- dynamic player_active/1.

:- dynamic card_in_possession/2.
% The player that the person running this program is playing (i.e. me)
:- dynamic player_controlled/1.
:- dynamic last_play_id/1.
:- dynamic recorded_suggestion/3.
:- dynamic recorded_player_response/3.


/*
    Game Setup
*/

setup_players([]).
setup_players([Player|Players]) :- 
    % Make sure to remove all previous active players before calling this function
    player(Player),
    assert(player_active(Player)),
    setup_players(Players).

setup_cards([]).
setup_cards([Card|Cards]) :-
    card(Card),
    player_controlled(Hero),
    assert(card_in_possession(Hero, Card)),
    setup_cards(Cards).

/*
    Main Gameplay
    This portion has to do with players doing suggestions
    and rebuttals
*/

new_suggestion(Player, suggestion(Location, Suspect, Weapon)) :-
    % Get a new ID for the suggestion
    new_play_id(Id),
    % Record it
    assert(recorded_suggestion(Id, Player, suggestion(Location, Suspect, Weapon))).

% This happens when someone responds to a move the controlling player makes
player_suggestion_response(SuggestionId, Opponent, DebunkingCard) :-
    % We know that the opponent possesses this card
    assert(card_in_possession(Opponent, DebunkingCard)),
    % Record this response
    assert(recorded_player_response(SuggestionId, Opponent, DebunkingCard)).

new_play_id(X) :- 
    last_play_id(LastId),
    X is LastId + 1,
    retract(last_play_id(_)),
    assert(last_play_id(X)).

/*
    Game Rules
*/

% Finding the next turn
next_player(CurrentPlayer, NextPlayer) :- 
    % Get the first relatively left player
    relatively_left(CurrentPlayer, NextPlayer),
    % That's active
    player_active(NextPlayer),
    % No more
    !.
    
% Player Ordering
left_of(missScarlet, colonelMustard).
left_of(colonelMustard, mrsWhite).
left_of(mrsWhite, mrGreen).
left_of(mrGreen, mrsPeacock).
left_of(mrsPeacock, professorPlum).
left_of(professorPlum, missScarlet).

relatively_left(LeftPlayer, RightPlayer) :- left_of(LeftPlayer, RightPlayer).
relatively_left(LeftPlayer, RightPlayer) :-
    % There is some players that connect LeftPlayer to RightPlayer
    left_of(LeftPlayer, DirectlyLeft),
    relatively_left(DirectlyLeft, RightPlayer).

% Players
player(missScarlet).
player(colonelMustard).
player(mrsWhite).
player(mrGreen).
player(mrsPeacock).
player(professorPlum).

% Weapons (Old school)
weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(lead_pipe).
weapon(wrench).

% Locations (Old school)
location(kitchen).
location(ballroom).
location(conservatory).
location(billiard_room).
location(library).
location(study).
location(hall).
location(lounge).
location(dining_room).

% Cards

card(Card) :- location(Card).
card(Card) :- weapon(Card).
card(Card) :- player(Card).
