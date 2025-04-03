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
%token LPAR RPAR 
%token CONST FUN REC VAR PROC 
%token LBRA RBRA
%token PV FL STAR DP VIR
%token IF AND OR
%token TBOOL INT
%token VARE ADR
%token ECHO SET IFI WHILE CALL

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.block> block
%type <Ast.block> prog

%start prog

%%
prog: block    { $1 }
;

block : LBRA cmds RBRA { ASTBlock($2) }
;

cmds:
    stat                  { [ASTStat $1] }
  | def PV cmds           { [ASTDef $1] @ $3 }
  | stat PV cmds          { [ASTStat $1] @ $3 }
;

def:
    CONST IDENT typec expr                  { ASTConst($2, $3, $4) }
  | FUN IDENT typec LBRA args RBRA expr     { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT typec LBRA args RBRA expr { ASTRec($3, $4, $6, $8) }
  | VAR IDENT typec                         { ASTVarD($2, $3) }
  | PROC IDENT LBRA argps RBRA block                 { ASTProc($2, $4, $6)}
  | PROC REC IDENT LBRA argps RBRA block                 { ASTPR($3, $5, $7)}
;

typec : 
    TBOOL                      { ASTBoolean }
  | INT                       { ASTInt }
  | LPAR types FL typec RPAR  { ASTTypeFleche($2, $4) }
;

types : typec             { [$1] }
  | typec STAR types       { [$1] @ $3 }
;

arg : IDENT DP typec      { ASTArg($1, $3) }
;

args : 
    arg                { [$1] }
  | arg VIR args          { [$1] @ $3 }
;

argps : 
    argp           { [$1]} 
  | argp VIR argps { [$1] @ $3 }
;

argp : 
    IDENT DP typec               {ASTArgB($1, $3)}
  | VARE IDENT DP typec          {ASTArgP($2, $4)}
;

stat:
    ECHO expr             { ASTEcho($2) }
  | SET IDENT expr        { ASTSet($2, $3) }
  | IFI expr block block  { ASTIfI($2, $3, $4) }
  | WHILE expr block      { ASTWhile($2, $3) }
  | CALL IDENT exprps      { ASTCall($2, $3) }
;

exprps : 
    exprp                       { [$1] }
  | exprp exprps                { [$1] @ $2 } 
; 

exprp : 
    expr                  { ASTExpr($1) }
  | LPAR ADR IDENT RPAR   { ASTAdr($3) }


expr:
    NUM                               { ASTNum($1) }
  | IDENT                             { ASTId($1) }
  | BOOL                              { ASTBool($1) }
  | LPAR IF expr expr expr RPAR       { ASTIf($3, $4, $5) }
  | LPAR AND expr expr RPAR           { ASTAnd($3, $4) }
  | LPAR OR expr expr RPAR            { ASTOr($3, $4) }
  | LPAR expr exprs RPAR              { ASTApp($2, $3) }
  | LBRA args RBRA expr               { ASTFermeture($2, $4) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

