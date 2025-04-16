type_var(G, X, T) :- G = [bind(X, T) | _].
type_var(G, X, T) :- G = [_ | G2],
    type_var(G2, X, T).


scnds([], []).
scnds(L, S) :- 
    L = [(_, Y) | LT],
    S = [Y| ST],
    scnds(LT, ST).

var_to_arg([], []).
var_to_arg([(vare(X), T) | LT], [(X, ref(T)) | ST]) :- var_to_arg(LT, ST).
var_to_arg([(X, T) | LT], [(X, T) | ST]) :-
    not(X = vare(_)),
    var_to_arg(LT, ST).

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

bt_prog(prog(Bl)) :-is_init_env(G),
    bt_block(G, Bl, T).

bt_block(G, block(Cs), T) :- bt_commande(G, Cs, T).

bt_commande(G, [D|Cs], T) :- bt_definition(G, D, G2), 
    bt_commande(G2, Cs, T).

bt_commande(G, [S|Cs], T) :-  bt_stat(G, S, T+void),
    bt_commande(G,Cs, T).

bt_commande (G, return(e), T) :- bt_expr(G, e, T).

bt_commande(G, [S|Cs], T) :-  bt_stat(G, S, void),
    bt_commande(G,Cs, T).

bt_commande(G, [S], T) :- bt_stat(G, S, T). 

bt_definition(G, const(X, T, E), [(X, T)|G]) :- bt_expr(G, E, T).

bt_definition(G, function(X, T, A, Bl), [(X, fun(TS, T))|G]) :- 
    append(A, G, G2),
    scnds(A, TS),
    bt_block(G2, Bl, T).
bt_definition(G, function_rec(X, T, A, Bl), [(X, fun(TS,T))|G]) :- 
    scnds(A, TS), 
    append(A, G, G2),
    bt_block([(X, fun(TS, T))| G2], Bl, T).

bt_definition(G, var(I, int), [(I, ref(int))| G]).
bt_definition(G, var(I, bool), [(I, ref(bool))|G]).

bt_definition(G, proc(I, A, B), [(I, fun(TS, void))|G]) :-
    var_to_arg(A, AS),
    append(AS, G, G2),
    scnds(AS, TS),
    bt_block(G2, B).
bt_definition(G, procrec(I, A, B), [(I, fun(TS, void))|G]) :-
    var_to_arg(A, AS),
    scnds(AS,TS),
    append(AS, G, G2),
    bt_block([(I, fun(TS, void))| G2], B).

bt_stat(G, echo(E), void) :- bt_expr(G, E, int).
bt_stat(G, set(I, E), void) :- 
    bt_expr(G, E, T), 
    bt_expr(G, E, T).
bt_stat(G, ifi(E, B1, B2), void) :- bt_expr(G, E, bool),
    bt_block(G, B1, void),
    bt_block(G, B2, void).
bt_stat(G, ifi(E, B1, B2), T+void) :- bt_expr(G, E, bool),
    bt_block(G, B1, T),
    bt_block(G, B2, void).
bt_stat(G, ifi(E, B1, B2), T+void) :- bt_expr(G, E, bool),
    bt_block(G, B1, void),
    bt_block(G, B2, T).
bt_stat(G, ifi(E, B1, B2), T) :- bt_expr(G, E, bool),
    bt_block(G, B1, T),
    bt_block(G, B2, T).
bt_stat(G, ifi(E, B1, B2), T+void) :- bt_expr(G, E, bool),
    bt_block(G, B1, T),
    bt_block(G, B2, void).
bt_stat(G, ifi(E, B1, B2), T+void) :- bt_expr(G, E, bool),
    bt_block(G, B1, void),
    bt_block(G, B2, T).
bt_stat(G, while(E, B), T+void) :- bt_expr(G, E, bool),
    bt_block(G, B, T).
bt_stat(G, call(I, ES), void) :- member((I, fun(TS, void)), G) ,
    bt_exprps(G, ES, TS).

bt_exprp(G, adr(X), ref(T)) :- member((X, ref(T)), G).
bt_exprp(G, E, T):-bt_expr(G, E, T).

bt_expr(G, alloc(X), vec(T)) :- bt_expr(G, X, int).
bt_expr(G, nthexpr(X, Y), T) :- 
    bt_expr(G, X, vec(T)),
    bt_expr(G, Y, int).
bt_expr(G, len(X), int) :- bt_expr(G, X, vec(T)).
bt_expr(G, vset(X, Y, Z), vec(T)) :- 
    bt_expr(G, X, vec(T)),
    bt_expr(G, Y, int),
    bt_expr(G, Z, T).

bt_expr(_, (num(_)), int).
bt_expr(G, id(X), T) :- member((X, T), G).
bt_expr(G, id(X), T) :- member((X, ref(T)), G).
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


bt_exprps(_, [], []).

bt_exprps(G, [E|ES], [T|TS]) :- bt_exprp(G, E, T),
    bt_exprps(G, ES, TS).

bt_exprs(G, [E|ES], [T|TS]) :- bt_expr(G, E, T),
    bt_exprs(G, ES, TS).

bt_exprs(_, [], []).

/* Commande d'execution : dune exec ./inter.exe ../../Samples/prog5.aps | swipl typage.pl*/

:- read(The_program),
    bt_prog(The_program).