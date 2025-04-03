%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token <bool> BOOL
%token <string> TYPE
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO, FUN, CONST, REC
%token INT, TBOOL, TYPE
%token IF, AND, OR, ARR, PV
%token DP 
%token VIR 
%token MUL 

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
    stat                   { [ASTStat $1] }
  | def PV cmds            { [ASTDef $1] @ $3}
;

def : 
    CONST IDENT typec expr                     { ASTConst($2, $3, $4) }
  | FUN IDENT typec LBRA args RBRA expr        { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT typec LBRA args RBRA expr    { ASTRec($3, $4, $6, $8) }
;

typec : INT                       { ASTInt }
  | TBOOL                         { ASTBoolean }
  | LPAR types ARR typec RPAR     { ASTTypeFleche($2, $4) }
;

types : 
    typec             { [$1] }
  | typec MUL types   { [$1] @ $3 } 
;

arg : IDENT DP typec { ASTArg($1, $3) }
;

args : 
    arg               { [$1] }
  | arg VIR args      { [$1] @ $3 }
;

stat: ECHO expr             { ASTEcho($2) }
;

expr:
    NUM                               { ASTNum($1) }
  | IDENT                             { ASTVar($1) }
  | BOOL                              { ASTBool($1) }
  | LPAR IF expr expr expr RPAR       { ASTIf($3, $4, $5) }
  | LPAR AND expr expr RPAR           { ASTAnd($3, $4) }
  | LPAR OR expr expr RPAR            { ASTOr($3, $4) }
  | LPAR expr exprs RPAR              { ASTApp($2, $3) }
  | LBRA args RBRA expr               { ASTFermeture($2, $4) }
;

exprs :
  expr          { [$1] }
| expr exprs    { $1::$2 }
;

