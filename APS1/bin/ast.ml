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

type block = 
  ASTBlock of cmd list
and typeAst = 
    ASTInt 
  | ASTBoolean
  | ASTTypeFleche of typeAst list * typeAst
and arg = 
  ASTArg of ident * typeAst
and expr =
    ASTNum of int
  | ASTId of ident
  | ASTBool of bool
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTFermeture of arg list * expr
and def = 
    ASTConst of ident * typeAst * expr
  | ASTFun of ident * typeAst * arg list * expr
  | ASTRec of ident * typeAst * arg list * expr
  | ASTVarD of ident * typeAst
  | ASTProc of ident * arg list * block 
  | ASTPR of ident * arg list * block
and stat =
    ASTEcho of expr
  | ASTSet of ident * expr
  | ASTIfI of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of ident * expr list
and cmd =
      ASTStat of stat
    | ASTDef of def
