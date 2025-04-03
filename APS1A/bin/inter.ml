open APS1A
open Ast

let rec print_expr e = 
  match e with 
  ASTNum n -> Printf.printf"num(%d)" n
  | ASTId x -> Printf.printf"id(%s)" x
  | ASTBool b -> Printf.printf"bool(%b)" b
  | ASTApp(e, es) -> (
    Printf.printf "app( ";
    print_expr e;
    Printf.printf ", [";
    print_exprs es;
    Printf.printf "] )";
  )
  | ASTIf(c, t, e) -> (
    Printf.printf "if( ";
    print_expr c;
    Printf.printf ", ";
    print_expr t;
    Printf.printf ", ";
    print_expr e;
    Printf.printf " )"
  )
  | ASTAnd(c1, c2) -> (
    Printf.printf "and(";
    print_expr c1;
    Printf.printf ", ";
    print_expr c2;
    Printf.printf " )";
  )
  | ASTOr(c1, c2) -> (
    Printf.printf "or(";
    print_expr c1;
    Printf.printf ", ";
    print_expr c2;
    Printf.printf " )";
  )
  | ASTFermeture(a1, e) -> (
    Printf.printf "abs( ";
    Printf.printf "[";
    print_args a1;
    Printf.printf "]";
    Printf.printf ",  ";
    print_expr e;
    Printf.printf ") "
  )
and print_exprs es = 
  match es with 
    [] -> ()
  | [e] -> print_expr e
  | e::es -> (
    print_expr e;
    print_string ", ";
    print_exprs es;
  )
and print_exprps es = 
  match es with 
    [] -> ()
  | [e] -> print_exprp e
  | e::es -> (
    print_exprp e;
    print_string ", ";
    print_exprps es;
  )
and print_exprp e = 
  match e with 
    ASTExpr e -> (
      print_expr e;
    )
  | ASTAdr a -> (
    Printf.printf "adr(%s)" a;
  )
and print_arg a = 
  match a with 
    ASTArg(ident, typeArg) -> ( 
      print_string "( ";
      print_string ident;
      print_string ", ";
      print_type typeArg;
      print_string ")"
    )
and print_args a1 =
  match a1 with 
    [] -> ()
  | a::[] -> print_arg a;
  | a::a1 -> (
    print_arg a;
    print_string ", ";
    print_args a1;
  )
and print_argp a = 
  match a with 
    ASTArgB(ident, typeArg) -> ( 
      print_string "( ";
      print_string ident;
      print_string ", ";
      print_type typeArg;
      print_string ")"
    )
  | ASTArgP(ident, typeArg) -> (
    print_string "( vare(";
    print_string ident;
    print_string "), ";
    print_type typeArg;
    print_string ")";
  )
and print_argps a1 =
  match a1 with 
    [] -> ()
  | a::[] -> print_argp a;
  | a::a1 -> (
    print_argp a;
    print_string ", ";
    print_argps a1;
  )
and print_type t =
  match t with 
    ASTInt -> (print_string "int";)
  | ASTBoolean -> (print_string "bool";)
  | ASTTypeFleche(ts, t) ->(
    Printf.printf("fun( [");
    print_types ts;
    Printf.printf "]";
    print_string ", ";
    print_type t;
    print_string ")";
  )
and print_types ts = 
  match ts with
      [] -> ()
    | t::ts -> (
      print_type t;
      print_string ", ";
      print_types ts;
    )
and print_def d = 
  match d with 
      ASTConst(i, t, e) -> (
      Printf.printf "const( ";
      print_string i;
      Printf.printf ", ";
      print_type t;
      Printf.printf ", ";
      print_expr e;
      Printf.printf ")";
    )
    | ASTFun(i, t, a1, e) -> (
      Printf.printf "function( ";
      print_string i;
      Printf.printf ", ";
      print_type t;
      Printf.printf ", [";
      print_args a1;
      Printf.printf "], ";
      print_expr e;
      print_string " )";
    )
    | ASTRec(i, t, a1, e) -> (
      Printf.printf "function_rec( ";
      print_string i;
      Printf.printf ", ";
      print_type t;
      Printf.printf ", [";
      print_args a1;
      Printf.printf "],";
      print_expr e;
      Printf.printf ")";
    ) 
    | ASTVarD(i, t) -> (
      Printf.printf "var( ";
      print_string i;
      Printf.printf ", ";
      print_type t;
      Printf.printf ")";
    )
    | ASTProc(i, args, b)->(
      Printf.printf "proc( ";
      print_string i;
      Printf.printf ", [";
      print_argps args;
      Printf.printf "], ";
      print_block b;
      Printf.printf ")";
    )
    | ASTPR(i, args, b)->(
      Printf.printf "procrec( ";
      print_string i;
      Printf.printf ",[ ";
      print_argps args;
      Printf.printf "], ";
      print_block b;
      Printf.printf ")";
    )
and print_stat s = 
  match s with 
      ASTEcho e -> (
	    Printf.printf("echo( ");
	    print_expr(e);
	    Printf.printf(" )");
    )
    | ASTSet(i, e) -> (
      Printf.printf("set( ");
      print_string i;
      Printf.printf(", ");
      print_expr e;
      Printf.printf(")");
    )
    | ASTIfI (e, b1, b2) -> 
      Printf.printf "ifi( ";
      print_expr e;
      Printf.printf ", ";
      print_block b1;
      Printf.printf ", ";
      print_block b2;
      Printf.printf " )";
    | ASTWhile (e, b) -> 
      Printf.printf "while( ";
      print_expr e;
      Printf.printf ", ";
      print_block b;
      Printf.printf ") ";
    | ASTCall (i, es) -> 
      Printf.printf "call( ";
      print_string i;
      Printf.printf ", [";
      print_exprps es; 
      Printf.printf "])";
and print_cmd c =
  match c with
    ASTStat s -> print_stat s
  | ASTDef d -> print_def d
and print_cmds cs =
  match cs with
      [] -> ()
    | c::[] -> print_cmd c
    | c::cs -> (
        print_cmd c;
        Printf.printf ", ";
        print_cmds cs;
      )
and print_block bl =
  match bl with 
    ASTBlock p -> (
      Printf.printf("block( [");
      print_cmds p;
      Printf.printf("] )")
    )
and print_prog p =
  Printf.printf ("prog( ");
  print_block p;
  Printf.printf ")";
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0