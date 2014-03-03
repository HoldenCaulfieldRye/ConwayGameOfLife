
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(war_of_life).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Question 2, Initial Board State:
	
	% [[ [1,1],[2,6],[3,4],[3,5],[3,8],[4,1],[4,2],[5,7],[6,2],[7,1],[7,3],[7,5] ], 
	%  [ [1,8],[2,2],[2,8],[3,7],[4,6],[5,3],[6,6],[7,6],[7,7],[7,8],[8,3],[8,7] ]]


% Question 2, 3rd generation Board State:

 	% [[ [1,6],[2,2],[2,4],[2,5],[2,6],[3,1],[3,8],[4,8],[5,1],[5,8],[8,4],[8,5]],
	%  [ [1,7],[2,8],[6,8],[7,6],[7,8],[8,7],[8,8]]] 


% Question 2, 4th generation Board State:

	% [[ [1,6],[2,5],[2,6],[3,5],[3,8],[4,7],[4,8],[5,7],[5,8],[7,5],[8,5]],
	%  [[1,7],[2,8],[6,8],[7,6],[7,8],[8,6],[8,7],[8,8]]]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
	infer_stat(Winner, Draws, P1Wins, P2Wins),
	Longest is TotalMoves,
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
infer_stat('draw', Draws, P1Wins, P2Wins):-
	Draws is 1,
	P1Wins is 0,
	P2Wins is 0.
infer_stat('stalemate', Draws, P1Wins, P2Wins):-
	Draws is 1,
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TEST THEM!

bloodlust(PlayerColour, CBState, NBState, Move):-
	all_possible_moves(PlayerColour, CBState, Moves),
	extract_max_subject_to(Moves, 'bloodlust', PlayerColour, CBState, NBState, Move, _).

land_grab(PlayerColour, CBState, NBState, Move):-
	all_possible_moves(PlayerColour, CBState, Moves),
	extract_max_subject_to(Moves, 'land_grab', PlayerColour, CBState, NBState, Move, _).

minimax(PlayerColour, CBState, NBState, Move):-
	all_possible_moves(PlayerColour, CBState, Moves),
	extract_max_subject_to(Moves, 'minimax', PlayerColour, CBState, NBState, Move, _).


%%%%%%%%%% helper predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% friends_and_foes/4: Given colour and boardstate, assigns friendly pieces and foe pieces.
%                     (Assumes notational convention of blues before reds is respected)

friends_and_foes('b', [AliveFriends, AliveFoes], AliveFriends, AliveFoes).
friends_and_foes('r', [AliveFoes, AliveFriends], AliveFriends, AliveFoes).

%%%%%%

% all_possible_moves/3: given colour and boardstate, finds all colour's possible moves.

all_possible_moves(PlayerColour, CurrentBoardState, Moves):-
	friends_and_foes(PlayerColour, CurrentBoardState, AliveFriends, AliveFoes),
	findall([R1, C1, R2, C2],
		(
		 member([R1, C1], AliveFriends),
		 one_move_away([R1, C1], [R2, C2]), 
		 \+ member([R2, C2], AliveFriends), 
		 \+ member([R2, C2], AliveFoes)
		),
		Moves).

%%%%%%

% one_move_away/3: returns true the 2 positions are adjacent on board map.

one_move_away([R1, C1], [R2, C2]):-
	RowDiff is R1 - R2,
	ColDiff is C1 - C2,
	RowDiff > -2,
	RowDiff < 2,
	ColDiff > -2,
	ColDiff < 2,
	\+ [R1, C1] is [R2, C2].

%%%%%%

% extract_max_subject_to/7: Finds element of Moves that maximises Criterion.
%                           Remembers the board state that results from this move and sets
%                           NewBoardState to it.

% base case for bloodlust
extract_max_subject_to([Move], 'bloodlust', PlayerColour, CBState, NBState, Move, Score):-
	alter_board(Move, CBState, NBState),
	friends_and_foes(PlayerColour, NBState, _, PotentialAliveFoes),
	Score is 50 - len(PotentialAliveFoes).
	% 50 is a 'high enough' constant to get >0 scores 

% base case for self preservation:
extract_max_subject_to([Move], 'self_preservation', PlayerColour, CBState, NBState, Move, Score):-
	alter_board(Move, CBState, NBState),
	friends_and_foes(PlayerColour, NBState, PotentialAliveFriends, _),
	Score is len(PotentialAliveFriends).

% base case for land grab:
extract_max_subject_to([Move], 'land_grab', PlayerColour, CBState, NBState, Move, Score):-
	alter_board(Move, CBState, NBState),
	friends_and_foes(PlayerColour, NBState, PotentialAliveFriends, PotentialAliveFoes),
	Score is len(PotentialAliveFriends) - len(PotentialAliveFoes).

extract_max_subject_to([H|T], Criterion, CBState, NewBoardState, Move, Score):-
	extract_max_subject_to(T, Criterion, CBState, NBStateA, MoveA, ScoreA),
	extract_max_subject_to([H], Criterion, CBState, NBStateB, MoveB, ScoreB),
	(
	 \+ ScoreA < ScoreB,
	 NewBoardState = NBStateA,
	 Move = MoveA,
	 Score = ScoreA
	;
	 ScoreA < ScoreB,
	 NewBoardState = NBStateB,
	 Move = MoveB,
	 Score = ScoreB
	).

%%%%%%

