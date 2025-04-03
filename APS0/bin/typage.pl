type_var(G, X, T) :- G = [bind(X, T) | _].
type_var(G, X, T) :- G = [_ | G2],
    type_var(G2, X, T).


scnds([], []).
scnds(L, S) :- 
    L = [(_, Y) | LT],
    S = [Y| ST],
    scnds(LT, ST).

is_init_env(G):-
    G = [(add, fun([int,int], int)),
        (ge, fun([int, int], bool)), 
        (eq, fun([int, int], bool)),
        (true, bool),
        (false, bool),
        (lt, fun([int, int], bool)),
        (sub, fun([int, int], int)),
        (mul, fun([int, int], int)),
        (div, fun([int, int], in))]. 

bt_prog(prog(Cs)) :-is_init_env(G),
    bt_commande(G, Cs).

bt_commande(G, [D|Cs]) :- bt_definition(G, D, G2), 
    bt_commande(G2, Cs).
    
bt_commande(G, [ST|_]) :- bt_stat(G, ST). 

bt_definition(G, const(X, T, E), [(X, T)|G]) :- bt_expr(G, E, T).
bt_definition(G, function(X, T, A, E), [(X, fun(TS, T))|G]) :- 
    append(A, G, G2),
    scnds(A, TS),
    bt_expr(G2, E, T).

bt_definition(G, function_rec(X, T, A, E), [(X, fun(TS,T))|G]) :- 
    scnds(A, TS), 
    append(A, G, G2),
    bt_expr([(X, fun(TS, T))| G2], E, T).

bt_stat(G, echo(E)) :- bt_expr(G, E, int).

bt_expr(_, (num(_)), int).
bt_expr(G, var(X), T) :- member((X, T), G).
bt_expr(_, bool(_), bool).

bt_expr(G, if(E1, E2, E3), T) :- bt_expr(G, E1, bool),
    bt_expr(G, E2, T),
    bt_expr(G, E3, T).

bt_expr(G, and(E1, E2), bool) :- bt_expr(G, E1, bool),
    bt_expr(G, E2, bool).

bt_expr(G, or(E1, E2), bool) :- bt_expr(G, E1, bool),
    bt_expr(G, E2, bool).


bt_expr(G, app(E, ES), T) :- bt_expr(G, E, fun(TS, T)),
    bt_exprs(G, ES, TS).
/*
bt_expr(G, E, T) :- 
    E = [(E1, T1)| ES],
    bt_expr(G, E1, T1),
    bt_expr(G, ES, T).
*/
bt_expr(G, abs(A, E), fun(TS, T)) :-
    append(A, G, G2),
    scnds(A, TS),
    bt_expr(G2, E, T).

bt_exprs(G, [E|ES], [T|TS]) :- bt_expr(G, E, T),
    bt_exprs(G, ES, TS).

bt_exprs(_, [], []).

/* Commande d'execution : dune exec ./inter.exe ../../Samples/prog5.aps | swipl typage.pl*/

:- read(The_program),
    bt_prog(The_program).