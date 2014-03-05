
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(war_of_life).
:- set_prolog_flag(toplevel_print_options, [max_depth(100)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Question 2, Initial Board State:
	
	% [[ [1,1],[2,6],[3,4],[3,5],[3,8],[4,1],[4,2],[5,7],[6,2],[7,1],[7,3],[7,5] ], 
	%  [ [1,8],[2,2],[2,8],[3,7],[4,6],[5,3],[6,6],[7,6],[7,7],[7,8],[8,3],[8,7] ]]


% Question 2, 3rd generation Board State:

 	% [[ [1,6],[2,2],[2,4],[2,5],[2,6],[3,1],[3,8],[4,8],[5,1],[5,8],[8,4],[8,5]],
	%  [ [1,7],[2,8],[6,8],[7,6],[7,8],[8,7],[8,8]]] 


% Question 2, 4th generation Board State:

	% [[ [1,6],[2,5],[2,6],[3,5],[3,8],[4,7],[4,8],[5,7],[5,8],[7,5],[8,5]],
	%  [[1,7],[2,8],[6,8],[7,6],[7,8],[8,6],[8,7],[8,8]]]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% test_strategy/3: 
%      Plays N war of life game with given strategies.
%      Prints stats to terminal.
test_strategy(N, P1Strat, P2Strat):-
	now(Start),
	play_many(N, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen),
	now(End),
	format('Number of draws: ~w ~n', [Draws]),
	format('Number of wins for player 1: ~w ~n', [P1Wins]),
	format('Number of wins or player 2: ~w ~n', [P2Wins]),
	format('Longest (non-exhaustive) game: ~w moves ~n', [Longest]),
	format('Shortest game: ~w moves ~n', [Shortest]),
	format('Average game length (including exhaustives): ~w moves ~n', [AvgLen]),
	format('Average game time: ~f seconds ~n~n', [(End - Start) / N]).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SUPPORT FOR test_strategy/3

%%%%%% play_many/3: 
%      Recursively plays war of life games given strategies and number of games.
%      Assigns stats to arguments.
%      Base case: play one game.
play_many(1, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen):-
	play(quiet, P1Strat, P2Strat, TotalMoves, Winner),
	write(Winner),
	infer_stat(Winner, Draws, P1Wins, P2Wins),
	(Winner = exhaust ->
	 Longest is 0              % ignore if exhaustive
	;
	 Longest is TotalMoves),
	Shortest is TotalMoves,
	AvgLen is TotalMoves.

%      Recursive case: recursive call, play one game, update stats.
play_many(N, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen):-
	N > 1,
	M is N-1,
	play_many(M, P1Strat, P2Strat, DrawsA, P1WinsA, P2WinsA, LongestA, ShortestA, AvgLenA),
	play_many(1, P1Strat, P2Strat, DrawsB, P1WinsB, P2WinsB, LongestB, ShortestB, AvgLenB),
	Draws is DrawsA + DrawsB,
	P1Wins is P1WinsA + P1WinsB,
	P2Wins is P2WinsA + P2WinsB,
	max(LongestA, LongestB, Longest),
	min(ShortestA, ShortestB, Shortest),
	update_avg(AvgLenA, AvgLenB, N, M, AvgLen).

%%%%%%
max(A, B, A):-
	A > B.
max(A, B, B):-
	\+ A > B.

%%%%%%
min(A, B, A):-
	A < B.
min(A, B, B):-
	\+ A < B.

%%%%%% update_avg/5:
%      Given an average over N-1 values, an N-th value, and a dummy Nminus1 index, updates the
%      average over N values.
%      Note the Nminus1 parameter may not be necessary nor elegant, but it is more efficient 
%      to include it here, since play_many already computes N-1, and update_avg is only ever
%      called from play_many. Otherwise, we would be computing it a 2nd time.
update_avg(Prev_Avg, Update, N, Nminus1, New_Avg):-
	New_Avg is (Update + (Nminus1 * Prev_Avg)) / N.

%%%%%%
infer_stat(Winner, Draws, P1Wins, P2Wins):-
	(Winner = 'draw'      -> (Draws is 1, P1Wins is 0, P2Wins is 0);
	 Winner = 'stalemate' -> (Draws is 1, P1Wins is 0, P2Wins is 0);
	 Winner = 'exhaust'   -> (Draws is 0, P1Wins is 0, P2Wins is 0);
	 Winner = 'b'         -> (Draws is 0, P1Wins is 1, P2Wins is 0);
	 Winner = 'r'         -> (Draws is 0, P1Wins is 0, P2Wins is 1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% STRATEGIES
%      Strategy predicates are all grouped here, because rather than hack at them separately, I
%      tried to write helper predicates used in all that do as much of the work as possible.

% An obvious flaw of bloodlust is that it has no concern for losses incurred by a move.
% An extreme  case would be a move that loses the game for player despite inflicting maximum 
% losses to opponent. A strong advantage of bloodlust is that it will always select a winning
% move, if such a move exists.
bloodlust(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'bloodlust', PlayerColour, CB, NB, Move, _).

% An obvious flaw of self_preservation is that it has no concern for losses inflicted to
% opponent. An extreme (and not so rare) case would be a move that wins the game for the player
% despite incurring more losses to player than another possible move. A small advantage is that
% it will never select a losing move. 
self_preservation(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'self_preservation', PlayerColour, CB, NB, Move, _).

% An obvious advantage of land_grab is that it takes both losses inflicted and losses incurred
% into account. However, a noticeable disadvantage is that unlike bloodlust, it will not
% necessarily select a winning move when such a move exists (consider a winning move that
% leaves player with only 1 piece, vs a move that leaves opponent with 5 pieces and player
% with 7 pieces). Another disadvantage is that it may select a losing move .. ?????
land_grab(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'land_grab', PlayerColour, CB, NB, Move, _).

minimax(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'minimax', PlayerColour, CB, NB, Move, _).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SUPPORT FOR STRATEGIES

%%%%%% extract_max_subject_to/7:
% Subject to a list of moves, a strategy, a player colour, and a board state, finds the move
% that is optimal for the strategy. Does so 
% base case for bloodlust
% it is not necessary nor elegant for next_board to provide friends/foes as well as new board
% state, but this redundancy is computationally more efficient.
extract_max_subject_to([Move], 'bloodlust', PlayerColour, CB, NB, Move, Score):-
	next_board(PlayerColour, CB, Move, _, NextAliveFoes, NB),
	length(NextAliveFoes, X),
	Score is 50 - X.
	% 50 is a 'high enough' constant to get >0 scores 

% base case for self preservation:
extract_max_subject_to([Move], 'self_preservation', PlayerColour, CB, NB, Move, Score):-
	next_board(PlayerColour, CB, Move, NextAliveFriends, _, NB),
	length(NextAliveFriends, Score).

% base case for land grab:
extract_max_subject_to([Move], 'land_grab', PlayerColour, CB, NB, Move, Score):-
	next_board(PlayerColour, CB, Move, NextAliveFriends, NextAliveFoes, NB),
	length(NextAliveFriends, X),
	length(NextAliveFoes, Y),
	Score is X - Y.

% base case for minimax:
% if player's move ends game, this predicate acts like normal land_grab.
% otherwise, given player's move, opponent will respond according to land_grab strategy.
% the resulting board state is used to compute a land_grab score for players' move.
extract_max_subject_to([Move], 'minimax', PlayerColour, CB, NB, Move, Score):-
	next_board(PlayerColour, CB, Move, NextAliveFriends, NextAliveFoes, NB),
	game_ended(NB, GameEnded),
	(
	 GameEnded = 'true' ->
	 length(NextAliveFriends, W),
	 length(NextAliveFoes, X),
	 Score is W - X
	;
	 opponent(PlayerColour, Opponent),
	 land_grab(Opponent, NB, FurtherB, _),
	 board_by_colour(PlayerColour, FurtherB, FurtherAliveFriends, FurtherAliveFoes),
	 length(FurtherAliveFriends, Y),
	 length(FurtherAliveFoes, Z),
	 Score is Y - Z
	).
	
% recursive case
extract_max_subject_to([H|T], Criterion, PlayerColour, CB, NB, Move, Score):-
	extract_max_subject_to(T, Criterion, PlayerColour, CB, NBa, MoveA, ScoreA),
	extract_max_subject_to([H], Criterion, PlayerColour, CB, NBb, MoveB, ScoreB),
	(\+ ScoreA < ScoreB ->
	 NB = NBa,
	 Move = MoveA,
	 Score = ScoreA
	;
	 NB = NBb,
	 Move = MoveB,
	 Score = ScoreB).

%%%%%%
%     Given colour and boardstate, assigns friendly pieces and foe pieces.
%     (Assumes notational convention of blues before reds is respected)
board_by_colour('b', [AliveFriends, AliveFoes], AliveFriends, AliveFoes).
board_by_colour('r', [AliveFoes, AliveFriends], AliveFriends, AliveFoes).

%%%%%%
%     Given colour and boardstate, finds all colour's possible moves.
all_possible_moves(PlayerColour, CurrentBoardState, Moves):-
	board_by_colour(PlayerColour, CurrentBoardState, AliveFriends, AliveFoes),
	findall([R1, C1, R2, C2],
		(
		 member([R1, C1], AliveFriends),
		 between(1, 8, R2),
		 between(1, 8, C2),
		 one_move_away([R1, C1], [R2, C2]), 
		 \+ member([R2, C2], AliveFriends), 
		 \+ member([R2, C2], AliveFoes)
		),
		Moves).

%%%%%%
%      Given Min, Max, can instantiate X to any value from Min to Max.
between(Min, _, Min).
between(Min, Max, X):-
	NewMin is Min+1,
	\+ NewMin > Max,
	between(NewMin, Max, X).


%%%%%%
%      Returns true the 2 positions are adjacent on board map.
one_move_away([R1, C1], [R2, C2]):-
	(R1 - R2) > -2,
	(R1 - R2) <  2,
	(C1 - C2) > -2,
	(C1 - C2) <  2,
	\+ [R1, C1] = [R2, C2].


%%%%%%
%      Given a player, a board, a move, sets player's state and opponent's state after Conway.
%      Note that the PlayerColour, NextAliveFriends, NextAliveFoes arguments are not necessary
%      for the execution of this predicate, but they increase efficiency by preventing having
%      to figure them out here or in extract_max_subject_to base case.
next_board(PlayerColour, CB, Move, NextAliveFriends, NextAliveFoes, NB):-
	board_by_colour(PlayerColour, CB, AliveFriends, AliveFoes),
	alter_board(Move, AliveFriends, InterimAliveFriends),
	board_by_colour(PlayerColour, InterimBoard, InterimAliveFriends, AliveFoes),
	next_generation(InterimBoard, NB),
	board_by_colour(PlayerColour, NB, NextAliveFriends, NextAliveFoes).


%%%%%%
opponent(PlayerColour, OpponentColour):-
	(PlayerColour = 'b' -> OpponentColour = 'r'; OpponentColour = 'b').


%%%%%%
game_ended([AliveBlues, AliveReds], GameEnded):-
	(AliveBlues = [] -> GameEnded = 'true'
	;
	 AliveReds = [] -> GameEnded = 'true'
	;
	 GameEnded = 'false').


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% END OF PROGRAM
