
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

% play war of life game N times
% prints results to chart
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


max(A, B, A):-
	A > B.
max(A, B, B):-
	\+ A > B.


min(A, B, A):-
	A < B.
min(A, B, B):-
	\+ A < B.


update_avg(Prev_Avg, Update, N, Nminus1, New_Avg):-
	New_Avg is (Update + (Nminus1 * Prev_Avg)) / N.


% helper predicate for play_many base case.
infer_stat(Winner , Draws, P1Wins, P2Wins):-
	(Winner = 'draw'      -> (Draws is 1, P1Wins is 0, P2Wins is 0)),
	(Winner = 'stalemate' -> (Draws is 1, P1Wins is 0, P2Wins is 0)),
	(Winner = 'exhaust'   -> (Draws is 0, P1Wins is 0, P2Wins is 0)),
	(Winner = 'b'         -> (Draws is 0, P1Wins is 1, P2Wins is 0)),
	(Winner = 'r'         -> (Draws is 0, P1Wins is 0, P2Wins is 1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% strategy predicates are all grouped here, because rather than hack at them separately, I
% tried to write helper predicates used in all that do as much of the work as possible.

bloodlust(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'bloodlust', PlayerColour, CB, NB, Move, _).

self_preservation(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'land_grab', PlayerColour, CB, NB, Move, _).

land_grab(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'land_grab', PlayerColour, CB, NB, Move, _).

minimax(PlayerColour, CB, NB, Move):-
	all_possible_moves(PlayerColour, CB, Moves),
	extract_max_subject_to(Moves, 'minimax', PlayerColour, CB, NB, Move, _).


%%%%%%%%%% helper predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% extract_max_subject_to/7:
% base case for bloodlust
% notice next_board directly provides friends/foes so no need to figure them out
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
% given player's move, opponent will respond according to land_grab strategy.
% the resulting board state is used to compute land_grab score for players' move.
extract_max_subject_to([Move], 'minimax', PlayerColour, CB, NB, Move, Score):-
	next_board(PlayerColour, CB, Move, _, _, InterimB),
	opponent(PlayerColour, Opponent),
	land_grab(Opponent, InterimB, NB, _),
	board_by_colour(PlayerColour, NB, NextAliveFriends, NextAliveFoes),
	length(NextAliveFriends, X),
	length(NextAliveFoes, Y),
	Score is X - Y.
	
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


% %%%%%% board_by_colour/4:
%        Given colour and boardstate, assigns friendly pieces and foe pieces.
%        (Assumes notational convention of blues before reds is respected)
board_by_colour('b', [AliveFriends, AliveFoes], AliveFriends, AliveFoes).
board_by_colour('r', [AliveFoes, AliveFriends], AliveFriends, AliveFoes).


%%%%%% all_possible_moves/3:
%      Given colour and boardstate, finds all colour's possible moves.
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


%%%%%% between/3: 
%      Given Min, Max, can instantiate X to any value from Min to Max.
between(Min, _, Min).
between(Min, Max, X):-
	NewMin is Min+1,
	\+ NewMin > Max,
	between(NewMin, Max, X).


%%%%%% one_move_away/3:
%      Returns true the 2 positions are adjacent on board map.
one_move_away([R1, C1], [R2, C2]):-
	(R1 - R2) > -2,
	(R1 - R2) <  2,
	(C1 - C2) > -2,
	(C1 - C2) <  2,
	\+ [R1, C1] = [R2, C2].


%%%%%% next_board/5:
%      Given a player, a board, a move, sets player's state and opponent's state after Conway.
%      Note that the PlayerColour, NextAliveFriends, NextAliveFoes arguments are not necessary
%      for the execution of this predicate, but they increase efficiency by preventing having to
%      figure them out here or in extract_max_subject_to base case.
next_board(PlayerColour, CB, Move, NextAliveFriends, NextAliveFoes, NB):-
	board_by_colour(PlayerColour, CB, AliveFriends, AliveFoes),
	alter_board(Move, AliveFriends, InterimAliveFriends),
	board_by_colour(PlayerColour, InterimBoard, InterimAliveFriends, AliveFoes),
	next_generation(InterimBoard, NB),
	board_by_colour(PlayerColour, NB, NextAliveFriends, NextAliveFoes).


%%%%%%%%%% tester predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% format('before ~w occurs: ~n ~w ~n', [Move, CB]), draw_board(CB),
% format('after ~w occurs, before Conway\'s Crank: ~n ~w ~n', [Move, InterimBoard]),
% draw_board(InterimBoard), show_score(verbose, InterimBoard),
% ...
% format('after Conway\'s Crank: ~w ~n', [NB]),
% draw_board(NB), show_score(verbose, InterimBoard).


%      Finds element of Moves that maximises Criterion.
%      Remembers the board state that results from this move and sets
%      NewBoardState to it.

% keep for debugging
	% format('extract_max([~w]) found~n', [Move]),
	% format('before ~w: ~n ~w ~n', [Move, CB]), draw_board(CB), show_score(verbose, CB),
	% format('~n after ~w: ~n ~w ~n', [Move, NB]), draw_board(NB), show_score(verbose, NB),
	% format('so Score = ~w ~n ~n', [Score]),

% CB
% [[[2,1],[2,6],[3,2],[3,3],[4,3],[4,8],[5,6],[5,8],[6,7],[8,1],[8,2],[8,6]],
%  [[1,2],[1,4],[2,7],[3,4],[4,1],[4,4],[4,5],[5,1],[5,3],[5,4],[6,3],[6,5]]],

% NB
% [[8,7],
%  [[2,1],[2,6],[3,2],[3,3],[4,3],[4,8],[5,6],[5,8],[6,7],[8,1],[8,2],[8,6]],
%  [[1,2],[1,4],[2,7],[3,4],[4,1],[4,4],[4,5],[5,1],[5,3],[5,4],[6,3],[6,5]]]

% alter_board([8,6,8,7], [[[2,1],[8,6]],[[1,2]]], [[8,7],[[2,1],[8,6]],[[1,2]]]).

% next_board('b', [[[2,1],[8,6]],[[1,2]]], [8,6,8,7], NextFriends, NextFoes, NB).	

% test_all_possible_moves(PlayerColour, Moves):-
% 	start_config(random, Board),
% 	format('~nInitial State:~n~n', []),
% 	draw_board(Board),
% 	show_score(verbose, Board),
% 	all_possible_moves(PlayerColour, Board, Moves).


% test_extract_max(PlayerColour, Criterion, Moves, MaxMove, MaxScore):-
% 	start_config(random, Board),
% 	format('~nInitial State:~n~n', []),
% 	draw_board(Board),
% 	show_score(verbose, Board),
% 	all_possible_moves(PlayerColour, Board, Moves),
% 	%trace,
% 	extract_max_subject_to(Moves, Criterion, PlayerColour, Board, NB, MaxMove, MaxScore),
% 	draw_board(NB),
% 	show_score(verbose, NB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	
	
