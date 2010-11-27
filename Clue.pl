% Game setup
:- dynamic player_active/1.

setup_players([Player|Players]) :- 
    % Make sure to remove all previous active players
    retractall(player_active(_)),
    player(Player),
    assert(player_active(Player)),
    setup_players(Players).

setup_players([]).



% Player Ordering
left_of(missScarlet, colonelMustard).
left_of(colonelMustard, mrsWhite).
left_of(mrsWhite, mrGreen).
left_of(mrGreen, mrsPeacock).
left_of(mrsPeacock, professorPlum).
left_of(professorPlum, missScarlet).

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
