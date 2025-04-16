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
  | ASTTab of typeAst
  | ASTTypeFleche of typeAst list * typeAst
and arg = 
  ASTArg of ident * typeAst
and argp = 
    ASTArgB of ident * typeAst
  | ASTArgP of ident * typeAst
and exprp = 
    ASTAdr of ident 
  | ASTExpr of expr
and expr =
    ASTNum of int
  | ASTId of ident
  | ASTBool of bool
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTFermeture of arg list * expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNthExpr of expr * expr
  | ASTVset of expr * expr * expr
and def = 
    ASTConst of ident * typeAst * expr
  | ASTFun of ident * typeAst * arg list * expr
  | ASTRec of ident * typeAst * arg list * expr
  | ASTVarD of ident * typeAst
  | ASTProc of ident * argp list * block 
  | ASTPR of ident * argp list * block
and stat =
    ASTEcho of expr
  | ASTSet of lval * expr
  | ASTIfI of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of ident * exprp list
and lval = 
    ASTLval of ident
  | ASTNth of lval * expr
and cmd =
      ASTStat of stat
    | ASTDef of def
