:- module(moduleIA, [minimax/5, eval_nb_move/3, eval_bord/2, eval_value/3, compute_points/3]).
:- use_module('Deplacement.pl').
:- use_module(library(lists)).


% eval_nb_move(+J, +P, ?NbM)
% retourne le nombre de coup NbC du joueur J pour le plateau P
eval_nb_move(J, P, NbM) :-
    findall(_, deplacement(J, P, _, _), ListeMove),
    length(ListeMove, NbM).


% eval_bord(+P, ?Value)
% Permet de déterminer l'avantage d'un des deux joueurs.
% Si la valeur de retour est positif, alors le joueur 1 à l'avantage. Et vise versa.
% La valeur maximale est abs(100).
eval_bord(P, 1000) :-
    gagner(1, P), !.
eval_bord(P, -1000) :-
    gagner(2, P), !.
eval_bord(P, V) :-
    eval_value(1, P, ValueJ1),
    eval_value(2, P, ValueJ2),
    V is ValueJ1 - ValueJ2.


% eval_value(+J, +P, ?Value)
% Retourne la valeur pour le joueur J
eval_value(J, P, Value) :-
    maplist(row(P), [1, 2, 3], Rows),
    maplist(compute_points(J), Rows, PRows),
    sumlist(PRows, PtsRows),
    maplist(column(P), [1, 2, 3], Cols),
    maplist(compute_points(J), Cols, PCols),
    sumlist(PCols, PtsCols),
    diagonal(P, 1, Prof1), compute_points(J, Prof1, PtsProf1),
    diagonal(P, 2, Prof2), compute_points(J, Prof2, PtsProf2),
    Value is PtsRows + PtsCols + PtsProf1 + PtsProf2.


% compute_points(+J, +List, ?Score)
% Distribut un score en fonction du nombre de pionts allignés.
compute_points(J, List, Score) :-
    include(=:=(J), List, Filtered),
    length(Filtered, Len),
    score(Len, Score).


% score(+NbPions, ?score)
% Retourne le score associé au nombre de pionts alignés.
score(0, 0) :- !.
score(1, 10) :- !.
score(2, 50) :- !.
score(3, 1000).

minimax(0, Plateau, Player, Value, _) :- 
    eval_value(Player, Plateau, Value).
minimax(D, Plateau, Player, Value, Move) :-
    D > 0, 
    D1 is D - 1,
    findall(M, deplacement(Player, Plateau, M, _), Moves),
    minimax(Moves, Plateau, D1, Player, -1000, nil, Value, Move).

minimax([], _, _, _, Value, Best, Value, Best).
minimax([Move|Moves],Plateau,D,Player, Value0,Move0,BestValue,BestMove):-
    deplacement(Player, Plateau, Move, Plateau1),
    Opponent is -Player,
    minimax(D, Plateau1, Opponent, OppValue, _OppMove), 
    Value is -OppValue,
    ( Value > Value0 ->        
        minimax(Moves,Plateau,D,Player, Value ,Move ,BestValue,BestMove)
    ;   minimax(Moves,Plateau,D,Player, Value0,Move0,BestValue,BestMove)
    ).
