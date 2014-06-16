:- module(moduleInterface, [initialisation/0, choixPartie/1, lancerPartie/1, afficherPlateau/1]).
:- use_module('Deplacement.pl').
:- use_module('IA.pl').

% Stock l'état du plateau
:- dynamic board/1.

% Initialisation du plateau à vide
initialisation:-
    retractall(board(_)),
    assert(board([0, 0, 0, 0, -2, 0, 0, 0, 0])),
    board(P),
    afficherCoordonnees(P).

 % Choix du type de partie
 % 0: Joueur/IA, 1: IA/IA, 2: Quitter
 % 0: Facile, 1: Moyen, 2: Difficile
choixPartie(Choix) :-
	writeln('Quel est votre choix ?'),
    read(Choix),
    integer(Choix),
    between(0, 2, Choix),
    !.
choixPartie(Choix) :-
    writeln('Choix invalide!'),
    choixPartie(Choix).

% Choix des Pions
% 0: 'O' commence la partie
% 1: 'X' 
choixPions(Choix) :-
    writeln('Quel est votre choix ?'),
    read(Choix),
    integer(Choix),
    between(0, 1, Choix),
    !.
choixPions(Choix) :-
    writeln('Choix invalide!'),
    choixPions(Choix).

% Lance la partie en fonction du choix du joueur
% 0: Joueur contre IA
% 1: IA contre IA
% 2: Quitter
lancerPartie(0) :-
    nl,
	writeln('Niveau (IA) :'),
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    choixPartie(Niveau),
    board(Plateau),
    Niveau1 is Niveau + 3,
    writeln('Choix Pions :'),
    writeln('\t0.\tO'),
    writeln('\t1.\tX'),
    choixPions(Pions),
    (Pions == 0 ->
        jouer(Plateau, Niveau1, 1)
    ;   jouerIA(Plateau, Niveau1, _DernierCoup, -1)
    ).
lancerPartie(1) :-
	nl,
    writeln('Niveau (IA1) :'),
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    choixPartie(Niveau1),
    Niveau11 is (Niveau1 + 1) * 2,
    nl,
    writeln('Niveau (IA2) :'),
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    choixPartie(Niveau2),
    board(Plateau),
    Niveau22 is (Niveau2 + 1) * 2,
    observerIA(Niveau11, Niveau22, Plateau, _DernierCoup).
lancerPartie(2) :- !.

% Au tour du joueur de jouer
jouer(Plateau, Niveau, Joueur) :-
    nl,
    afficherPlateau(Plateau),
    writeln('A vous de jouer!'),
    effectuerAction(Coup),
    sauvegarderCoup(Joueur, Coup),
    board(NouveauPlateau),
    afficherPlateau(NouveauPlateau),
    not(gagner(Joueur, NouveauPlateau)),
    Adversaire is -Joueur,
    jouerIA(NouveauPlateau, Niveau, Coup, Adversaire).

% Au tour de l'IA de jouer
jouerIA(Plateau, Niveau, DernierCoup, Joueur) :-
    nl,
    board(_PlateauTemporaire),
    nombrePions(Joueur, Plateau, Nombre),
    (Nombre == 0 ->
        listeVide(Plateau, Liste),
        random_member(Index, Liste),
        sauvegarderCoup(Joueur, [-1, Index, 0])
    ;   minimax(Niveau, Plateau, Joueur, _Value, Coup, DernierCoup),
        sauvegarderCoup(Joueur, Coup)
    ),
    board(NouveauPlateau),
    afficherPlateau(NouveauPlateau),
    writeln('L\'IA vient de jouer!'),
    not(gagner(Joueur, NouveauPlateau)), !,
    Adversaire is -Joueur,
    jouer(NouveauPlateau, Niveau, Adversaire).

% IA vs. IA
observerIA(Niveau1, Niveau2, Plateau, DernierCoup) :-
    nl,
    % IA1
    board(_PlateauTemporaire),
    nombrePions(1, Plateau, Nombre),
    (Nombre == 0 ->
        listeVide(Plateau, Liste),
        random_member(Index, Liste),
        sauvegarderCoup(1, [-1, Index, 0]),
        Coup = [-1, Index, 0]
    ;   minimax(Niveau1, Plateau, 1, _Value, Coup, DernierCoup),
        sauvegarderCoup(1, Coup)
    ),
    board(NouveauPlateau),
    writeln('IA1'),
    afficherPlateau(NouveauPlateau),
    not(gagner(1, NouveauPlateau)), !,
    nl,
    % IA2
    board(_PlateauTemporaire2),
    nombrePions(-1, NouveauPlateau, Nombre1),
    (Nombre1 == 0 ->
        listeVide(NouveauPlateau, Liste1),
        random_member(Index1, Liste1),
        sauvegarderCoup(-1, [-1, Index1, 0]),
        Coup1 = [-1, Index1, 0]
    ;   minimax(Niveau2, NouveauPlateau, -1, _Value1, Coup1, Coup),
        sauvegarderCoup(-1, Coup1)
    ),
    board(NouveauPlateau2),
    writeln('IA2'),
    afficherPlateau(NouveauPlateau2),
    not(gagner(-1, NouveauPlateau2)), !,
    observerIA(Niveau1, Niveau2, NouveauPlateau2, Coup1).

% Demande le type de placement ainsi que ses coordonnées à l'utlisateur
% C1: Case d'origine, C2: Case de destination, Choix: Type d'action
effectuerAction([C1, C2, Choix]) :-
    nl,
    writeln('Liste des actions :'),
    writeln('\t0.\tPoser un pion'),
    writeln('\t1.\tD\u00E9placer un pion'),
    writeln('\t2.\tD\u00E9placer le taquet d\u0027une case'),
    writeln('\t3.\tD\u00E9placer le taquet de deux cases'),
    choixAction(Choix),
    (Choix == 0 -> C1 is -1, choixCase(C2, 'destination');
    Choix == 1 -> choixCase(C1, 'origine'), choixCase(C2, 'destination');
    Choix == 2 -> choixCase(C1, 'origine'), choixCase(C2, 'destination');
    Choix == 3 -> choixCase(C1, 'origine'), choixCase(C2, 'destination')).

 % Choix du type de placement
 % 0: Poser pion, 1: Déplacer pion, 2: Déplacer d'une case le taquet, 3: Déplacer de deux cases le taquet
choixAction(Choix) :-
    writeln('Quel est votre choix ?'),
    read(Choix),
    integer(Choix),
    between(0, 3, Choix),
    !.
choixAction(Choix) :-
    writeln('Choix invalide!'),
    choixAction(Choix).

% Demande la case d'origine
% C: Case, Type: Origine/Destination
choixCase(C, Type) :-
    write('Coordonn\u00E9es '), write(Type), nl,
    read(C),
    integer(C),
    between(0, 8, C), 
    !.
choixCase(C, Type) :-
    writeln('Valeur invalide!'),
    choixCase(C, Type).

% Sauvegarde le coup effectué
sauvegarderCoup(Joueur, Coup) :-
    retract(board(Plateau)),
    deplacement(Joueur, Plateau, Coup, NouveauPlateau),
    assert(board(NouveauPlateau)).

% Affiche les coordonnées du plateau
afficherCoordonnees([C0, C1, C2, C3, C4, C5, C6, C7, C8]):-
    nl,
	write('+-+-+-+     +-+-+-+'), nl,
	write('|'), afficherCase(C0), write('|'), afficherCase(C1), write('|'), afficherCase(C2), write('|     |0|1|2|'), nl,
	write('+-+-+-+     +-+-+-+'), nl,
	write('|'), afficherCase(C3), write('|'), afficherCase(C4), write('|'), afficherCase(C5), write('| <=> |3|4|5|'), nl,
	write('+-+-+-+     +-+-+-+'), nl,
	write('|'), afficherCase(C6), write('|'), afficherCase(C7), write('|'), afficherCase(C8), write('|     |6|7|8|'), nl,
	write('+-+-+-+     +-+-+-+'), nl.

% Affiche le contenu du plateau
afficherPlateau([C0, C1, C2, C3, C4, C5, C6, C7, C8]):-
	write('+-+-+-+'), nl,
	write('|'), afficherCase(C0), write('|'), afficherCase(C1), write('|'), afficherCase(C2), write('|'), nl,
	write('+-+-+-+'), nl,
	write('|'), afficherCase(C3), write('|'), afficherCase(C4), write('|'), afficherCase(C5), write('|'), nl,
	write('+-+-+-+'), nl,
	write('|'), afficherCase(C6), write('|'), afficherCase(C7), write('|'), afficherCase(C8), write('|'), nl,
	write('+-+-+-+'), nl.

% Affiche le contenu d'une case
afficherCase(-2) :-
    write('#').
afficherCase(0) :-
    write(' ').
afficherCase(1) :-
    write('o').
afficherCase(-1):-
    write('x').