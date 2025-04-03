(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type ident = string  

type typeAst = 
    ASTInt 
  | ASTBoolean
  | ASTTypeFleche of typeAst list * typeAst

type arg = 
  ASTArg of ident * typeAst

type expr =
    ASTNum of int
  | ASTVar of ident
  | ASTBool of bool
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTFermeture of arg list * expr

type def = 
    ASTConst of ident * typeAst * expr
  | ASTFun of ident * typeAst * arg list * expr
  | ASTRec of ident * typeAst * arg list * expr

type stat =
    ASTEcho of expr
      
type cmd =
    ASTStat of stat
    | ASTDef of def

	
