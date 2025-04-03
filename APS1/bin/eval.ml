open APS1
open Ast

exception Foo of string

module StringMap = Map.Make(String);;
module IntMap = Map.Make(Int);;

type envMap = (aps_valeur StringMap.t)
and aps_valeur =
    InZ of int
  | InA of int
  | InP of block * arg list * envMap
  | InPR of block * arg list * ident * envMap
  | InF of expr * arg list * envMap
  | InFR of expr * arg list * ident * envMap
  | InPrim of ((aps_valeur list) -> aps_valeur )

let pAct = ref 0
let mem = ref (IntMap.empty:(aps_valeur IntMap.t))

let aps_not = function 
  | [InZ(a)] -> 
    if a = 1 
      then InZ(0) 
      else InZ(1)
  | _ -> assert false 
  
let aps_eq = function 
  | [InZ(a); InZ(b)] -> 
    if a = b
      then InZ(1)
      else InZ(0)
  | _ -> assert false


let aps_lt = function
  | [InZ(a); InZ(b)] -> 
    if a < b 
      then InZ(1) 
      else InZ(0) 
  | _ -> assert false

let aps_add = function
  | [InZ(a); InZ(b)] -> InZ(a+b)
  | _ -> assert false

let aps_sub = function
  | [InZ(a); InZ(b)] -> InZ(a-b)
  | _ -> assert false


let aps_mul = function 
  | [InZ(a); InZ(b)] -> InZ(a*b)
  | _ -> assert false

let aps_div = function 
  | [InZ(a); InZ(b)] -> InZ(a/b)
  | _ -> assert false

let alloc_mem ()= 
  mem := IntMap.add !pAct (InZ(-99999)) !mem;
  pAct := !pAct + 1

let maj_mem p v = 
  mem := IntMap.update p v !mem

let init_Env_Primi env =
  let env = StringMap.add ("add") (InPrim(aps_add)) env in
  let env = StringMap.add "sub" (InPrim(aps_sub)) env in 
  let env = StringMap.add "div" (InPrim(aps_div)) env in
  let env = StringMap.add "mul" (InPrim(aps_mul)) env in
  let env = StringMap.add "not" (InPrim(aps_not)) env in
  let env = StringMap.add "lt" (InPrim(aps_lt)) env in
  StringMap.add "eq" (InPrim(aps_eq)) env  

let rec eval_expr e (env:(envMap)) = 
  match e with 
    ASTNum n -> InZ(n)
  | ASTId x -> (
    match StringMap.find_opt (x) (env) with 
        Some(InA(x)) -> IntMap.find x !mem
      | Some(x) -> x
      | None -> raise (Foo "Error var not in env")) 
  | ASTBool b ->
    if b = true
      then InZ(1)
      else InZ(0) 
  | ASTApp(e, es) -> (     
    match (eval_expr e env) with 
      InF(e', args, env') -> 
        (*Printf.printf "Appel \n";
        print_expr e;
        Printf.printf " ";
        print_exprs es;
        pp_map env;*)
        let env' = (match_args args es env env') in 
          (*pp_map env;*)
          eval_expr e' env';
      | InPrim(x) -> x (eval_exprs es env)
      | InFR(e', args, f, env') ->(
        (*Printf.printf "AppelRec \n";
        print_expr e;
        Printf.printf " ";
        print_exprs es;*)
        let env' = (match_args args es env env') in  
          (*Printf.printf "\nEnv Avant Matching :\n";
          pp_map env;*) 
          let env' = StringMap.add f (InFR(e', args, f, env')) env' in 
            (*Printf.printf "Env Apres matching :\n";
            pp_map env;
            Printf.printf "\n";*)
            eval_expr e' env')
      | InZ (_) -> raise (Foo "Error")
      | _ -> assert false
  )
  | ASTIf(c, t, e) -> (
    match (eval_expr c env) with
      InZ (x) ->
        if (x = 1)
          then (eval_expr t env)
          else (eval_expr e env)
    | _ -> raise(Foo "Error"))
  | ASTAnd(c1, c2) ->
    if (eval_expr c1 env) = InZ(0) 
      then InZ(0)
      else if (eval_expr c2 env) = InZ(0) 
        then InZ(0)
        else InZ(1) 
  | ASTOr(c1, c2) ->
    if (eval_expr c1 env) = InZ(0) 
      then InZ(0)
      else if (eval_expr c2 env) = InZ(0) 
        then InZ(0)
        else InZ(1) 
  | ASTFermeture(a1, e) -> 
    InF(e, a1, env)  
and match_args args es env1 env2 = 
  match args with 
    [] -> env2
  | (ASTArg(i, _))::argss -> 
    match es with 
      [] -> assert false
    | e1::ess -> (
        let env2 = (StringMap.add i (eval_expr e1 env1) env2) in 
        match_args argss ess env1 env2
    )
and eval_exprs es env = 
  match es with 
    [] -> assert false
  | [e1] -> 
      [eval_expr e1 env] 
  | e1::es1 -> 
      [eval_expr e1 env] @ eval_exprs es1 env 
and eval_def d (env:(envMap))= 
  match d with
      ASTConst(i, _, e) -> StringMap.add i (eval_expr e env) env 
    | ASTVarD(i, _) -> 
      let env = StringMap.add i (InA(!pAct)) env in
        alloc_mem ();
        env
    | ASTFun(i, _, a1, e) -> StringMap.add i (InF(e, a1, env)) env
    | ASTProc(i, a1, bk) -> StringMap.add i (InP(bk, a1, env)) env
    | ASTRec(i, _, a1, e) -> StringMap.add i (InFR(e, a1, i, env)) env
    | ASTPR (i, a1, bk) -> StringMap.add i (InPR(bk, a1, i, env)) env
  
and eval_stat s (env:(envMap)) fs = 
  match s with 
      ASTEcho e ->( 
        match (eval_expr e env) with 
          InZ(x) -> fs@[x]
          | _ -> assert false
          )
    | ASTSet(i, e) -> (
        match (StringMap.find_opt i env) with 
          Some(InA(p)) -> (
            let res = eval_expr e env in 
              match res  with 
                  InZ(_) ->  maj_mem p (Option.map (fun _ -> res)); fs
                | _ -> assert false 
          )
          | _ -> assert false 
      )
    | ASTIfI(e, bk1, bk2) -> (
        match (eval_expr e env) with 
            InZ(1) ->  
              let (_, fsS) = (eval_block bk1 env) in 
                fs@fsS
          | InZ(0) -> 
              let (_, fsS) = eval_block bk2 env in 
                fs@fsS
          | _ -> assert false
    )
    | ASTWhile(e, bk) -> (
        let res = eval_expr e env in 
          match(res) with 
              InZ(0) -> fs
            | InZ(1) -> 
                let(_, fsS) = eval_block bk env in 
                  fs @ (eval_stat (ASTWhile(e,bk)) env fsS)
            | _ -> assert false
    )
    | ASTCall(i, exprs) -> (
        match (StringMap.find i env) with 
            InP(bk, args, env')-> 
              let (_, fsS) = (eval_block bk (match_args args exprs env env')) in
                fs@fsS
          | InPR(bk, args, i, env') as procrec-> 
            let env = StringMap.add i procrec env in 
              let (_, fsS) =(eval_block bk (match_args args exprs env env')) in 
              fs@fsS
          | _ -> assert false
          )
and eval_cmd c env fs =
  match c with
    ASTStat s -> (env, (eval_stat s env fs))
  | ASTDef d -> (eval_def d env, fs)
and eval_cmds cs env fs =
  match cs with
      [] -> (env, fs)
    | [c] -> eval_cmd c env fs
    | c::cs -> let (env, fss) = eval_cmd c env fs in 
      eval_cmds cs env fss  
and  print_Stat f = 
  match f with 
    [] -> ()
  | [f1] -> print_endline (string_of_int f1)
  | f1::f2 -> print_endline (string_of_int f1); 
    print_Stat f2
and eval_prog p env = 
  let (_, fs) = eval_block p env in 
    print_Stat fs 
and eval_block bl env = 
  match bl with 
    ASTBlock (cmds) -> 
      let(env, t) = eval_cmds cmds env [] in
        (env,t)

let _ = 
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    eval_prog p (init_Env_Primi (StringMap.empty: envMap))
  with Lexer.Eof ->
    exit 0