% Game setup
:- dynamic player_active/1.

setup_players([Player|Players]) :- 
    % Make sure to remove all previous active players before calling this function
    player(Player),
    assert(player_active(Player)),
    setup_players(Players).

setup_players([]).

% Finding the next turn

% This is the case where there are inactive players between
% CurrentPlayer and NextPlayer
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
