:- module(moduleDeplacement, [deplacement/4, gagner/2]).

% Poser un pion
% Plateau = -1: taquet, 0: libre, 1: joueur1, 2: joueur2
% Coup = [COrig, Cdest, TypePlacement]
deplacement(Joueur, [0|R], [-1, 0, 0], [Joueur|R]) :-
    nombrePions(Joueur, [0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, 0|R], [-1, 1, 0], [C0, Joueur|R]) :-
    nombrePions(Joueur, [C0, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, 0|R], [-1, 2, 0], [C0, C1, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, 0|R], [-1, 3, 0], [C0, C1, C2, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, C3, 0|R], [-1, 4, 0], [C0, C1, C2, C3, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, C3, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, C3, C4, 0|R], [-1, 5, 0], [C0, C1, C2, C3, C4, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, C3, C4, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, C3, C4, C5, 0|R], [-1, 6, 0], [C0, C1, C2, C3, C4, C5, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, C3, C4, C5, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, C3, C4, C5, C6, 0|R], [-1, 7, 0], [C0, C1, C2, C3, C4, C5, C6, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, C3, C4, C5, C6, 0|R], Nombre),
    Nombre < 3.
deplacement(Joueur, [C0, C1, C2, C3, C4, C5, C6, C7, 0|R], [-1, 8, 0], [C0, C1, C2, C3, C4, C5, C6, C7, Joueur|R]) :-
    nombrePions(Joueur, [C0, C1, C2, C3, C4, C5, C6, C7, 0|R], Nombre),
    Nombre < 3.

% Déplacer un pion
% Plateau = -1: taquet, 0: libre, 1: joueur1, 2: joueur2
% Coup = [COrig, Cdest, 1]
deplacement(Joueur, Plateau, [CaseOrigine, CaseDestination, 1], NouveauPlateau) :-
    getCasesAdjacentes(CaseOrigine, CaseDestination),
    nth0(CaseOrigine, Plateau, Joueur),
    nth0(CaseDestination, Plateau, 0),
    setTableau(0, CaseOrigine, Plateau, NouveauPlateauTmp),
    setTableau(Joueur, CaseDestination, NouveauPlateauTmp, NouveauPlateau).

% Déplacer le taquet d'une case
% Plateau = -1: taquet, 0: libre, 1: joueur1, 2: joueur2
% Coup = [COrig, Cdest, 2]
deplacement(_, Plateau, [CaseOrigine, CaseDestination, 2], NouveauPlateau) :-
    getTaquin1(CaseOrigine, CaseDestination),
    nth0(CaseOrigine, Plateau, -1),
    setTableau(-1, CaseDestination, Plateau, NouveauPlateauTmp),
    nth0(CaseDestination, Plateau, Valeur),
    setTableau(Valeur, CaseOrigine, NouveauPlateauTmp, NouveauPlateau).

% Déplacer le taquet de deux cases
% Plateau = -1: taquet, 0: libre, 1: joueur1, 2: joueur2
% Coup = [COrig, Cdest, 3]
deplacement(_, Plateau, [CaseOrigine, CaseDestination, 3], NouveauPlateau) :-
    getTaquin2(CaseOrigine, CaseDestination),
    nth0(CaseOrigine, Plateau, -1),
    setTableau(-1, CaseDestination, Plateau, NouveauPlateauTmp),
    nth0(CaseDestination, Plateau, Valeur),
    setTableau(Valeur, CaseOrigine, NouveauPlateauTmp, NouveauPlateau).

% Retourne le nombre de pions d'un joueur
nombrePions(Joueur, Plateau, Nombre) :-
    sublist(=(Joueur), Plateau, Liste),
    length(Liste, Nombre).

% Modification du plateau
setTableau(Valeur, 0, [_|R], [Valeur|R]) :- !.
setTableau(Valeur, Case, [X|R1], [X|R2]) :-
    Case > 0,
    Case1 is Case - 1,
    setTableau(Valeur, Case1, R1, R2).

% Retourne les cases adjacentes
getCasesAdjacentes(0,1).
getCasesAdjacentes(0,3).
getCasesAdjacentes(1,0).
getCasesAdjacentes(1,2).
getCasesAdjacentes(1,4).
getCasesAdjacentes(2,1).
getCasesAdjacentes(2,5).
getCasesAdjacentes(3,0).
getCasesAdjacentes(3,4).
getCasesAdjacentes(3,6).
getCasesAdjacentes(4,1).
getCasesAdjacentes(4,3).
getCasesAdjacentes(4,5).
getCasesAdjacentes(4,7).
getCasesAdjacentes(5,2).
getCasesAdjacentes(5,4).
getCasesAdjacentes(5,8).
getCasesAdjacentes(6,3).
getCasesAdjacentes(6,7).
getCasesAdjacentes(7,4).
getCasesAdjacentes(7,6).
getCasesAdjacentes(7,8).
getCasesAdjacentes(8,5).
getCasesAdjacentes(8,7).

% Retourne les cases où le taquet peut déplacer une case
getTaquin1(0, 1).
getTaquin1(0, 3).
getTaquin1(1, 0).
getTaquin1(1, 2).
getTaquin1(1, 4).
getTaquin1(2, 1).
getTaquin1(2, 5).
getTaquin1(3, 0).
getTaquin1(3, 4).
getTaquin1(3, 6).
getTaquin1(4, 1).
getTaquin1(4, 3).
getTaquin1(4, 5).
getTaquin1(4, 7).
getTaquin1(5, 2).
getTaquin1(5, 4).
getTaquin1(5, 8).
getTaquin1(6, 3).
getTaquin1(6, 7).
getTaquin1(7, 4).
getTaquin1(7, 6).
getTaquin1(7, 8).
getTaquin1(8, 5).
getTaquin1(8, 7).

% Retourne les cases où le taquet peut déplacer deux cases
getTaquin2(0, 2).
getTaquin2(0, 6).
getTaquin2(1, 7).
getTaquin2(2, 0).
getTaquin2(2, 8).
getTaquin2(3, 5).
getTaquin2(5, 3).
getTaquin2(6, 0).
getTaquin2(6, 8).
getTaquin2(7, 1).
getTaquin2(8, 2).
getTaquin2(8, 6).

% Trois pions alignés sur une ligne
gagner(Joueur, [Joueur,Joueur,Joueur,_,_,_,_,_,_]) :- 
    Joueur \= 0, 
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).
gagner(Joueur, [_,_,_,Joueur,Joueur,Joueur,_,_,_]) :- 
    Joueur \= 0,
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).
gagner(Joueur, [_,_,_,_,_,_,Joueur,Joueur,Joueur]) :- 
    Joueur \= 0,
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).

% Trois pions align\u00E9s sur une colonne
gagner(Joueur, [Joueur,_,_,Joueur,_,_,Joueur,_,_]) :- 
    Joueur \= 0,
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).
gagner(Joueur, [_,Joueur,_,_,Joueur,_,_,Joueur,_]) :- 
    Joueur \= 0, 
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).
gagner(Joueur, [_,_,Joueur,_,_,Joueur,_,_,Joueur]) :- 
    Joueur \= 0, 
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).

% Trois pions align\u00E9s sur une diagonale
gagner(Joueur, [Joueur,_,_,_,Joueur,_,_,_,Joueur]) :- 
    Joueur \= 0, 
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).
gagner(Joueur, [_,_,Joueur,_,Joueur,_,Joueur,_,_]) :- 
    Joueur \= 0, 
    writef('Le joueur %w a gagn\u00E9!', [Joueur]).