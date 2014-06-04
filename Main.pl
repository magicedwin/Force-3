:- use_module('Interface.pl').

% Affiche le menu
main :-
    writeln('+---------------------+'),
    writeln('|  0.\tJoueur VS. IA |'),
    writeln('|  1.\tIA VS. IA     |'),
    writeln('+---------------------+'),
    writeln('|  2.\tQuitter       |'),
    writeln('+---------------------+'),
    initialisation,
    choixPartie(Choix),
    lancerPartie(Choix).