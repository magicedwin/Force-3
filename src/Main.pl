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

% Agrandissement des piles
:- set_prolog_stack(global, limit(100 000 000 000)).
:- set_prolog_stack(trail,  limit(20 000 000 000)).
:- set_prolog_stack(local,  limit(2 000 000 000)).