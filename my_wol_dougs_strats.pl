:- include('war_of_life.pl').
:- use_module(library(system)).
:- set_prolog_flag(toplevel_print_options, [max_depth(100)]).

%------ Question 2 -------------------------%
%
% The prolog board representation in question one is given by the list:
%	
%	[[ [1,1],[2,6],[3,4],[3,5],[3,8],[4,1],[4,2],[5,7],[6,2],[7,1],[7,3],[7,5] ], 
%	 [ [1,8],[2,2],[2,8],[3,7],[4,6],[5,3],[6,6],[7,6],[7,7],[7,8],[8,3],[8,7] ]]
%
%
% For the 3rd generation I found the board configuration to be:
%
% 	[[ [1,6],[2,2],[2,4],[2,5],[2,6],[3,1],[3,8],[4,8],[5,1],[5,8],[8,4],[8,5]],
%	 [ [1,7],[2,8],[6,8],[7,6],[7,8],[8,7],[8,8]]] 
%
% For the 4th generation:
%
%	[[ [1,6],[2,5],[2,6],[3,5],[3,8],[4,7],[4,8],[5,7],[5,8],[7,5],[8,5]],
%	 [[1,7],[2,8],[6,8],[7,6],[7,8],[8,6],[8,7],[8,8]]]
%
%--------------------------------------------%

%------ Question 3 --------------------------%
%
% test_strategy(N, S1, S2) : N is the desired number of games to be run, S1 is the
% strategy of player 1 and S2 the strategy of player 2.
%--------------------------------------------%

play(ShowFlag, FirstPlayerStrategy, SecondPlayerStrategy, TotalMoves, Winner, Board) :-
 (
  ShowFlag == verbose,
  format('~nInitial State:~n~n', []),
  draw_board(Board),
  show_score(verbose, Board)
  ;
  ShowFlag == quiet
 ),
 !,
 make_move(Board, ShowFlag, _, 'b', FirstPlayerStrategy, 'r', SecondPlayerStrategy, 0, TotalMoves, Winner).


test_strategy(NumGames, Strategy1, Strategy2) :-
    now(Start),
    iterateGames(NumGames, Strategy1, Strategy2, Draws, BlueWins, RedWins, LongestGame, ShortestGame, AvgLen),
    now(End),
    format('Number of draws: ~d ~n', [Draws]),
    format('Number of Blue wins: ~d ~n', [BlueWins]),
    format('Number of Red wins: ~d ~n', [RedWins]),
    format('Longest (non-exhaustive) game: ~d moves ~n', [LongestGame]),
    format('Shortest game: ~d moves ~n', [ShortestGame]),
    format('Average game length: ~d moves ~n', [AvgLen/ NumGames]),
    format('Average game time: ~f seconds ~n', [(End - Start) / NumGames]).

iterateGames(0, _, _, 0, 0, 0, 0, 250, 0).

/*
iterateGames(1, Strategy1, Strategy2, Draws, BlueWins, RedWins, LongestGame, ShortestGame, AvgLen) :-
    play(quiet, Strategy1, Strategy2, NumMoves, WinningPlayer),
    (WinningPlayer = 'r' -> (RedWins is 1, BlueWins is 0, Draws is 0); true),
    (WinningPlayer = 'b' -> (RedWins is 0, BlueWins is 1, Draws is 0); true),
    (WinningPlayer = 'draw' -> (RedWins is 0, BlueWins is 0, Draws is 1); true),
    (WinningPlayer = 'stalemate' -> (RedWins is 0, BlueWins is 0, Draws is 1); true),
    (WinningPlayer = 'exhaust' -> (LongestGame is 0); (LongestGame is NumMoves)), 
    ShortestGame is NumMoves,
    AvgLen is NumMoves.
*/

iterateGames(NumGames, Strategy1, Strategy2, Draws, BlueWins, RedWins, LongestGame, ShortestGame, AvgLen) :-
    NumGames > 0,
    Next is NumGames - 1,
    play(quiet, Strategy1, Strategy2, NumMoves, WinningPlayer),
    iterateGames(Next, Strategy1, Strategy2, NumDraws, BWins, RWins, LGame, SGame, AvLen),
    (WinningPlayer = 'r' -> (RedWins is (RWins + 1), BlueWins is BWins, Draws is NumDraws); true),
    (WinningPlayer = 'b' -> (RedWins is RWins, BlueWins is (BWins + 1), Draws is NumDraws); true),
    (WinningPlayer = 'draw' -> (RedWins is RWins, BlueWins is BWins, Draws is (NumDraws + 1)); true),
    (WinningPlayer = 'stalemate' -> (RedWins is RWins, BlueWins is BWins, Draws is (NumDraws + 1)); true),
    (WinningPlayer = 'exhaust' -> (LongestGame is LGame) ; (NumMoves > LGame -> (LongestGame is NumMoves) ; (LongestGame is LGame))),
    (NumMoves < SGame -> (ShortestGame is NumMoves) ; (ShortestGame is SGame)),
    AvgLen is (AvLen + NumMoves).


%----------- PART 2 -----------%

% Helper Functions:

%--- Return the possible move for a certain colour given a board state:

evalMoves(Colour, [Blues, Reds], PossMoves) :-
	(Colour = 'b' ->
	     getPossMoves(Blues, Reds, PossMoves) ;
             getPossMoves(Reds, Blues, PossMoves)).

getPossMoves(Alive, OtherPlayerAlive, PossMoves) :-
	findall([A,B,MA,MB], (member([A,B], Alive),
                       neighbour_position(A,B,[MA,MB]),
	               \+member([MA,MB],Alive),
	               \+member([MA,MB],OtherPlayerAlive)),
	     PossMoves).

%--- Return the new board position given a player's move:

evalNewBoard(Colour, [Blues, Reds], Move, NewBoardState) :-
	(Colour = 'b' ->
             (alter_board(Move, Blues, New), 
	      NewBoardState = [New, Reds]) ;
             (alter_board(Move, Reds, New), 
	      NewBoardState = [Blues, New])).

%--- Return the colour of the next player to move:

switchColour(Current, Next) :-
    (Current = 'b' -> Next = 'r'; Next = 'b').

%------------------------------------------
%
% bloodlust/4:
% Inputs are: current player colour, the current board state, the new board (after the move)
% and the move (which is to be determined).
%
%------------------------------------------

bloodlust(Colour, BoardState, NewBoardState, Move) :-
	evalMoves(Colour, BoardState, PossMoves),
	trace,
	evaluateBL(Colour, BoardState, PossMoves, 65, _, Move),
        evalNewBoard(Colour, BoardState, Move, NewBoardState).

% evaluateBL('b', [[[2,5],[3,4],[4,4],[4,5],[4,7],[5,2],[6,5],[6,8],[7,1],[7,6],[8,3],[8,4]],[[2,1],[2,6],[3,6],[5,6],[6,1],[6,6],[7,3],[7,4],[7,5],[7,7],[7,8],[8,5]]], [[2,5,1,4],[2,5,1,5],[2,5,1,6],[2,5,2,4],[2,5,3,5],[3,4,2,3],[3,4,2,4],[3,4,3,3],[3,4,3,5],[3,4,4,3],[4,4,3,3],[4,4,3,5],[4,4,4,3],[4,4,5,3],[4,4,5,4],[4,4,5,5],[4,5,3,5],[4,5,4,6],[4,5,5,4],[4,5,5,5],[4,7,3,7],[4,7,3,8],[4,7,4,6],[4,7,4,8],[4,7,5,7],[4,7,5,8],[5,2,4,1],[5,2,4,2],[5,2,4,3],[5,2,5,1],[5,2,5,3],[5,2,6,2],[5,2,6,3],[6,5,5,4],[6,5,5,5],[6,5,6,4],[6,8,5,7],[6,8,5,8],[6,8,6,7],[7,1,6,2],[7,1,7,2],[7,1,8,1],[7,1,8,2],[7,6,6,7],[7,6,8,6],[7,6,8,7],[8,3,7,2],[8,3,8,2]], 65, _, Move).

% evalNewBoard('b', [[[2,5],[3,4],[4,4],[4,5],[4,7],[5,2],[6,5],[6,8],[7,1],[7,6],[8,3],[8,4]],[[2,1],[2,6],[3,6],[5,6],[6,1],[6,6],[7,3],[7,4],[7,5],[7,7],[7,8],[8,5]]], [5,2,6,3], NewBoardState).

evaluateBL(_, _, [], _, Move, Move).

%--- Cycle through the possible moves for a given position and apply the
%--- bloodlustEval function. Return the one with the least value returned
%--- (we wish to minimise the opponents pieces!).

evaluateBL(Colour, BoardState, [Move|PossMoves], Record, BestMove, Final) :-
    bloodlustEval(Colour, BoardState, Move, Value),
    (Value < Record ->
        evaluateBL(Colour, BoardState, PossMoves, Value, Move, Final) ;
        evaluateBL(Colour, BoardState, PossMoves, Record, BestMove, Final)).

%--- Apply the move and count the number of pieces left on the opponent's side.

bloodlustEval(Colour, BoardState, Move, Value) :-
    evalNewBoard(Colour, BoardState, Move, NewBoardState),
    next_generation(NewBoardState, [Blue, Red]),
    (Colour = 'b' ->
         length(Red, Value) ;
         length(Blue, Value)).

%------------------------------------------
%
% self_preservation/4:
% Inputs are: current player colour, the current board state, the new board (after the move)
% and the move (which is to be determined).
%
%------------------------------------------

self_preservation(Colour, BoardState, NewBoardState, Move) :-
	evalMoves(Colour, BoardState, PossMoves),
	evaluateSP(Colour, BoardState, PossMoves, -65, _, Move),
        evalNewBoard(Colour, BoardState, Move, NewBoardState).

evaluateSP(_, _, [], _, Move, Move).

%--- Same as the bloodlust function, apply moves and return the highest scoring -
%--- we wish to maximise our own pieces this time.

evaluateSP(Colour, BoardState, [Move|PossMoves], Record, BestMove, Final) :-
    selfpresEval(Colour, BoardState, Move, Value),
    (Value > Record ->
        evaluateSP(Colour, BoardState, PossMoves, Value, Move, Final) ;
        evaluateSP(Colour, BoardState, PossMoves, Record, BestMove, Final)).

selfpresEval(Colour, BoardState, Move, Value) :-
    evalNewBoard(Colour, BoardState, Move, NewBoardState),
    next_generation(NewBoardState, [Blue, Red]),
    (Colour = 'b' ->
         length(Blue, Value) ;
         length(Red, Value)).

%------------------------------------------
%
% land_grab/4:
% Inputs are: current player colour, the current board state, the new board (after the move)
% and the move (which is to be determined).
%
%------------------------------------------

land_grab(Colour, BoardState, NewBoardState, Move) :-
	evalMoves(Colour, BoardState, PossMoves),
	evaluateLG(Colour, BoardState, PossMoves, -65, _, Move),
        evalNewBoard(Colour, BoardState, Move, NewBoardState).

evaluateLG(_, _, [], _, Move, Move).

%--- Apply the move and find the difference between our pieces and the opponents.
%--- Return the move that gives the highest difference.

evaluateLG(Colour, BoardState, [Move|PossMoves], Record, BestMove, Final) :-
    landgrabEval(Colour, BoardState, Move, Value),
    (Value > Record ->
        evaluateLG(Colour, BoardState, PossMoves, Value, Move, Final) ;
        evaluateLG(Colour, BoardState, PossMoves, Record, BestMove, Final)).

landgrabEval(Colour, BoardState, Move, Value) :-
    evalNewBoard(Colour, BoardState, Move, NewBoardState),
    next_generation(NewBoardState, [Blue, Red]),
    length(Blue, BlueCount),
    length(Red, RedCount),
    (Colour = 'b' ->
        Value is (BlueCount - RedCount) ;
        Value is (RedCount - BlueCount)).


%------------------------------------------
%
% minimax/4:
% Inputs are: current player colour, the current board state, the new board (after the move)
% and the move (which is to be determined). Searched to a depth of two moves.
%
%------------------------------------------


minimax(Colour, BoardState, NewBoardState, Move) :-
	evalMoves(Colour, BoardState, PossMoves),
	maxMin(Colour, BoardState, PossMoves, [_, -65], [Move, _]),
	evalNewBoard(Colour, BoardState, Move, NewBoardState).

maxMin(_,_,[],[Move, Value], [Move, Value]).

%--- Generate a new board for each move generated in 'minimax' and call the minMax function.
%--- Since minMax calls land_grab for the opponent, it will find the opponent's best move
%--- given the current board position. We suppose that the opponent will choose this move,
%--- so I return the negative of this value, which represents the damage that their move will
%--- do our pieces. I then find the move on our side that maximises these (mostly negative)
%--- values and choose that as the optimum move.

maxMin(Colour, BoardState, [Move|PossMoves], [BestMove, Record], [FinalMove, FinalVal]) :-
	evalNewBoard(Colour, BoardState, Move, NewBoardState),
	next_generation(NewBoardState, [NewBlues, NewReds]),
	switchColour(Colour, Next),
	evalMoves(Next, [NewBlues, NewReds], NewPossMoves),
	length(NewBlues, AliveBlues),
	length(NewReds, AliveReds),

        (isDead(Colour, AliveBlues, AliveReds, Score) ->
	     (Score > Record ->
	         maxMin(Colour, BoardState, PossMoves, [Move, Score], [FinalMove, FinalVal]) ;
      	         maxMin(Colour, BoardState, PossMoves, [BestMove, Record], [FinalMove, FinalVal])) ;
	
	     (minMax(Next, [NewBlues, NewReds], NewPossMoves, [_, -65], [_, Value]),
	     (Value > Record ->
	         maxMin(Colour, BoardState, PossMoves, [Move, Value], [FinalMove,FinalVal]) ;
      	         maxMin(Colour, BoardState, PossMoves, [BestMove, Record], [FinalMove, FinalVal])))).

%--- Return the negated value as we wish to represent the damage done to the player maximising.

minMax(_,_,[],[Move, Value],[Move, -Value]).

%--- Scan through the moves and apply the land grab evaluation function.

minMax(Colour, BoardState, [Move|PossMoves], [BestMove, Record], [FinalMove, FinalVal]) :-
	landgrabEval(Colour, BoardState, Move, Value),
	(Value > Record ->
	    minMax(Colour, BoardState, PossMoves, [Move, Value], [FinalMove, FinalVal]) ;
      	    minMax(Colour, BoardState, PossMoves, [BestMove, Record], [FinalMove, FinalVal])).

isDead(Colour, BlueCount, RedCount, Value) :-
	(BlueCount = 0; RedCount = 0),
	(Colour = 'b' ->
	     Value is (BlueCount - RedCount) ;
	     Value is (RedCount - BlueCount)).

