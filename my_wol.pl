
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- consult(war_of_life).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% play war of life game N times
% prints results to chart
test_strategy(N, P1Strat, P2Strat):-
	play_many(N, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen, AvgTime),
	show_stat('draws', Draws),
	show_stat('p1wins', P1Wins),
	show_stat('p2wins', P2Wins),
	show_stat('longest', Longest),
	show_stat('shortest', Shortest),
	show_stat('avglen', AvgLen),
	show_stat('avgtime', AvgTime).


% play_many helper predicate for test_strategy.
% a recursive predicate for repeatedly playing games and keeping track of stats.
play_many(1, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen, AvgTime):-
	play(quiet, P1Strat, P2Strat, TotalMoves, Winner),
	infer_stat(Winner, Draws, P1Wins, P2Wins),
	Longest is TotalMoves,
	Shortest is TotalMoves,
	AvgLen is TotalMoves,
	AvgTime is TotalMoves.

play_many(N, P1Strat, P2Strat, Draws, P1Wins, P2Wins, Longest, Shortest, AvgLen, AvgTime):-
	N > 1,
	M is N-1,
	play_many(M, P1Strat, P2Strat, DrawsA, P1WinsA, P2WinsA, LongestA, ShortestA, AvgLenA, AvgTimeA),
	play_many(1, P1Strat, P2Strat, DrawsB, P1WinsB, P2WinsB, LongestB, ShortestB, AvgLenB, AvgTimeB),
	Draws is DrawsA + DrawsB,
	P1Wins is P1WinsA + P1WinsB,
	P2Wins is P2WinsA + P2WinsB,
	max(LongestA, LongestB, Longest),
	min(ShortestA, ShortestB, Shortest),
	update_avg(AvgLenA, AvgLenB, N, M, AvgLen),
	update_avg(AvgTimeA, AvgTimeB, N, M, AvgTime).

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
infer_stat('draw', Draws, P1Wins, P2Wins):-
	Draws is 1,
	P1Wins is 0,
	P2Wins is 0.
infer_stat('exhaust', Draws, P1Wins, P2Wins):-
	write('EXHAUST!'),
	Draws is 0,
	P1Wins is 0,
	P2Wins is 0.
infer_stat('b', Draws, P1Wins, P2Wins):-
	Draws is 0,
	P1Wins is 1,
	P2Wins is 0.
infer_stat('r', Draws, P1Wins, P2Wins):-
	Draws is 0,
	P1Wins is 0,
	P2Wins is 1.

% show_stat helper predicate for test_strategy
show_stat('draws', Num):-
	format('Number of draws: ~w ~n', [Num]).

show_stat('p1wins', Num):-
	format('Number of wins for player 1: ~w ~n', [Num]).

show_stat('p2wins', Num):-
	format('Number of wins or player 2: ~w ~n', [Num]).

show_stat('longest', Num):-
	format('Longest (non-exhaustive) game: ~w ~n', [Num]).

show_stat('shortest', Num):-
	format('Shortest game: ~w ~n', [Num]).

show_stat('avglen', Num):-
	format('Average game length (including exhaustives): ~w ~n', [Num]).

show_stat('avgtime', Num):-
	format('Average game time: ~w ~n~n', [Num]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for play() to implement the strategies, the following predicates need to be modified to execute
% the new parameter values:
% move_piece(Player, Strategy, Board, NewBoard, Move),
% oh but they've already been written 



% Brute force approach to this would be: for each friendly piece, for each possible move by that
% piece, count number of opponent pieces left after doing it, and keep the one that maximises this
% count. However, notice that if a piece is not neighbouring an opponent piece, and a given move
% would not bring it to neighbourhood of an opponent piece, then such a move will make no difference
% to the number of opponent pieces resulting from that move. So it can be ignored, unless the player
% cannot make any move that will make a difference to the resulting total number of opponent's
% pieces.
bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move):-
	[AliveBlues, AliveReds] = CurrentBoardState,
	(
	 PlayerColour = 'b',
	 all_possible_moves(AliveBlues, AliveReds, Moves)
	;
	 PlayerColour = 'r',
	 all_possible_moves(AliveReds, AliveBlues, Moves)
	),
	blood_max_move(PlayerColour, Moves, CurrentBoardState, NewBoardState, Move).


blood_max_move('b', [Move], _, NewBoardState, Move):-
	[AliveBlues, AliveReds] = CurrentBoardState,
	alter_board(Move, AliveReds, NewAliveReds),
	alter_board(Move, AliveBlues, NewAliveBlues), % won't work! predicate doesn't know colours
	% need a predicate who given a blue move can calculate new reds - does one exist?
	next_generation([], [NewAliveBlues, NewAliveReds]

blood_max_move('b', [Move|OtherMoves], CurrentBoardState, NewBoardState, BestMove):- % next_generation to
	[AliveBlues, AliveReds] = CurrentBoardState,
	alter_board(Move, AliveBlues, NewAliveBlues),
	(
	 next_generation(Board, [NewAliveBlues, NewAliveReds])
	 ;
	),
	


all_possible_moves(AliveFriends, AliveOpponents, Moves):-
	findall([R1, C1, R2, C2],
		(
		 member([R1, C1], AliveFriends),
		 one_move_away([R1, C1], [R2, C2]), 
		 \+ member([R2, C2], AliveFriends), 
		 \+ member([R2, C2], AliveOpponents)
		),
		Moves).


one_move_away([R1, C1], [R2, C2]):-
	RowDiff is R1 - R2,
	ColDiff is C1 - C2,
	RowDiff > -2,
	RowDiff < 2,
	ColDiff > -2,
	ColDiff < 2,
	\+ [R1, C1] is [R2, C2].
	



	
self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move).
land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move).
minimax(PlayerColour, CurrentBoardState, NewBoardState, Move).