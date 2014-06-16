:- module(moduleIA, [minimax/6]).
:- use_module('Deplacement.pl').
:- use_module(library(lists)).

% evalPoints(+Joueur, +Plateau, ?Valeur)
% Retourne la valeur du plateau pour un joueur
evalPoints(Joueur, Plateau, Valeur) :-
    maplist(ligne(Plateau), [1, 2, 3], Lignes),
    maplist(calculPoints(Joueur), Lignes, ListePtsLignes),
    sumlist(ListePtsLignes, PtsLignes),
    maplist(colonne(Plateau), [1, 2, 3], Colonnes),
    maplist(calculPoints(Joueur), Colonnes, ListePtsColonnes),
    sumlist(ListePtsColonnes, PtsColonnes),
    diagonale(Plateau, 1, Diagonale1), calculPoints(Joueur, Diagonale1, PtsDiagonale1),
    diagonale(Plateau, 2, Diagonale2), calculPoints(Joueur, Diagonale2, PtsDiagonale2),
    Valeur is PtsLignes + PtsColonnes + PtsDiagonale1 + PtsDiagonale2.


% calculPoints(+Joueur, +Liste, ?Points)
% Calcul le score en fonction du nombre de pions alignés.
calculPoints(Joueur, Liste, Points) :-
    include(=:=(Joueur), Liste, ListePions),
    length(ListePions, Longueur),
    score(Longueur, Points).


% score(+NbPions, ?Ccore)
% Retourne le score associé au nombre de pionts alignés.
score(0, 0) :- !.
score(1, 10) :- !.
score(2, 50) :- !.
score(3, 1000).

% minimax(+Profondeur, +Plateau, +Joueur, +Valeur, ?Coup, ?DernierCoup)
% Algorithme MinMax afin de déterminer le meilleur coupp à jouer
minimax(0, Plateau, Joueur, Valeur, _Coup, _DernierCoup) :- 
    evalPoints(Joueur, Plateau, Valeur).
minimax(D, Plateau, Joueur, Valeur, Coup, DernierCoup) :-
    D > 0, 
    D1 is D - 1,
    findall(M, deplacement(Joueur, Plateau, M, _), Coups1),
    coupInverse(DernierCoup, Inverse),
    delete(Coups1, Inverse, Coups),
    minimax(Coups, Plateau, D1, Joueur, -1000, nil, Valeur, Coup).

% minimax(+ListeCoups, Plateau, Profondeur, Joueur, +ValeurCourante, +CoupCourant, ?BestValeur, ?BestCoup)
minimax([], _, _, _, Valeur, Best, Valeur, Best).
minimax([Coup|Coups],Plateau,D,Joueur, Valeur0,Coup0,BestValeur,BestCoup):-
    deplacement(Joueur, Plateau, Coup, Plateau1),
    Opponent is -Joueur,
    minimax(D, Plateau1, Opponent, OppValeur, _OppCoup, Coup),
    evalPoints(Joueur, Plateau1, ValeurJoueur),
    Valeur is ValeurJoueur - OppValeur,
    ( Valeur > Valeur0 ->        
        minimax(Coups,Plateau,D,Joueur, Valeur ,Coup ,BestValeur,BestCoup)
    ;   minimax(Coups,Plateau,D,Joueur, Valeur0,Coup0,BestValeur,BestCoup)
    ).