open EidosAST
open EidosTypes

exception Undefined
exception UnboundVar of string
exception IfExpr of string
exception ForExcept of string
exception WhileExcept of string
exception TernaryExcept of string
exception BoolEvalExcept of string
exception DebugVal of string

(* interpblock : env -> interp_block -> env*eidosValue*)  (* return last value of the block*)
let rec interpBlock env (int_b : interp_block) = match int_b with
                      | Empty      -> (env, Void)
                      | StmtInterp(stmt, int_b1) ->
                                                 let (new_env, _) = interpStatement env stmt in
                                                    interpBlock new_env int_b1
                      (*| FuncInterp(func_decl, int_b1) ->
                                                  let (new_env, _) = interpFuncDecl env func_decl in
                                                    interpBlock new_env int_b1
                        *)

(* interpStatement : env -> statement -> env*eidosValue *)
and interpStatement env (stmt : statement) = match stmt with
                                    | Cstmt cmpd_stmt -> interpCompoundStmt env cmpd_stmt
                                    | ExprStmt exp_stmt -> interpExprStmt env exp_stmt
                                    | For forstmt -> interpForStmt env forstmt
                                    | Do do_while_stmt -> interpDoWhileStmt env do_while_stmt
                                    | While whl -> interpWhileStmt env whl
                                    | Jump jstmt -> interpJumpStmt env jstmt
                                    | SlctStmt selec -> interpSelectStmt env selec

(* interpCompoundStmt : env -> compound_stmt -> env*eidosValue *)
and interpCompoundStmt env (cmpd : compound_stmt) = match cmpd with
                                           |CmpdStmt []      -> (env, Void)
                                           |CmpdStmt (s::ss) ->
                                                             let (new_env, value) = interpStatement env s in
                                                             interpCompoundStmt new_env (CmpdStmt ss)


(* interpExprStmt : env -> expr_stmt -> env*eidosValue *)
and interpExprStmt env (expr : expr_stmt) = match expr with
                               |Estmt ass_expr -> match ass_expr with
                                         None            -> (env, Void)
                                        |Some ass_expr_e -> interpAssignExpr env ass_expr_e

(* interpSelectStmt : env -> select_stmt -> env*eidosValue *)
and interpSelectStmt env (slct : select_stmt) = match slct with
                               | If (e, cmpd, v) -> match v with
                                                | None -> let (new_env, cond) = interpExpr env e in
                                                                (match cond with
                                                                       | (Logical boolean) -> if boolean == Array.of_list [true] then
                                                                                   interpCompoundStmt new_env cmpd else
                                                                                   (new_env, Void)
                                                                       | _ -> raise (IfExpr "expr inside if statement must be a boolean!"))
                                                |(Some cmpd1) -> let (new_env, cond) = interpExpr env e in
                                                                      (match cond with
                                                                      | (Logical boolean) -> if boolean == Array.of_list [true] then
                                                                                         interpCompoundStmt new_env cmpd else
                                                                                         interpCompoundStmt new_env cmpd1
                                                                      | _ -> raise (IfExpr "expr inside if statement must be a boolean!"))


(* interpForStmt : env -> for_stmt -> env*eidosValue *)
and interpForStmt env (forStmt : for_stmt) = match forStmt with
                         |ForStmt (str, expr, stmt) ->
                         (* let rec loop e = *)
                                                             let (new_env, value) = interpExpr env expr in (* probably a try catch to recover from errors! *)
                                                             (match value with
                                                              | Void -> (new_env, Void)
                                                              | _    -> interp_for_value env str value stmt (interpStatement))
                                                              (* let (env1, _) = interpStatement (Env.add str value e) stmt in *)
                                                                        (* loop env1) in
                                                               loop env *)


(* interpDoWhileStmt : env -> do_while_stmt -> env*eidosValue *)
and interpDoWhileStmt env (dowhile : do_while_stmt) = match dowhile with
                         | DoWhile (stmt, expr)  ->
                                               let (new_env, value) = interpStatement env stmt in
                                                let (env1, cond) = interpExpr new_env expr in
                                                  (match cond with
                                                    |Logical boolean -> if boolean == Array.of_list [false] then
                                                                            (new_env, value) else
                                                                         interpWhileStmt new_env (WhileStmt (expr, stmt))
                                                    | _ -> raise (WhileExcept "expr inside while statement must evaluate to a boolean!"))



(* interpWhileStmt : env -> while_stmt -> env*eidosValue *) (* Implement loop unrolling! *)
and interpWhileStmt env (whileStmt : while_stmt) = match whileStmt with
                                          | WhileStmt (expr, stmt) ->
                                                     interpSelectStmt env
                                                                  (If (expr, CmpdStmt [stmt]
                                                                    , Some
                                                                    (CmpdStmt
                                                                      [While
                                                                      (WhileStmt (expr,stmt))
                                                                      ]
                                                                    )))


(* interpJumpStmt : env -> jump_stmt -> env*eidosValue *)
and interpJumpStmt env (jump : jump_stmt):env*eidosValue = match jump with (* this might be very wrong! *)
                      |Next  -> (env, Void)
                      |Break -> (env, Void)
                      |Return None -> (env, Void)
                      |Return (Some expr) -> interpExpr env expr

(* interpExpr : env -> expr -> env*eidosValue *)
and interpExpr env (E cond_expr : expr) = interpConditionalExpr env cond_expr

(* interpAssignExpr : env -> assign_expr -> env*eidosValue *)
(*and interpAssignExpr env (ass : assign_expr) = (* This implementation is wrong! just an attempt to get sth working : *)
                                 match ass with
                                 | Assign (cond, cond_exp_opt) -> let (new_env, value) = interpConditionalExpr env cond in (* value is string! *)
                                                                        print_string ((string_of_eidos_val value)^"\n" );
                                                                        (match cond_exp_opt with
                                                                        | None       -> (match value with
                                                                              String str -> let strArr = Array.to_list str in
                                                                                        (match strArr with
                                                                                         | []     -> raise (DebugVal "variable name should not be empty")
                                                                                         | (x::xs) -> (print_string "Added with no value to env\n");
                                                                                                      (Env.add x Void env, Void))
                                                                              | _          -> (new_env,value))
                                                                        | Some cond1 ->
                                                                                let (env1, value1) = interpConditionalExpr new_env cond1 in
                                                                                print_string ("left hand side: "^(string_of_eidos_val value)^"\n" );
                                                                                print_string ("right hand side: "^(string_of_eidos_val value1)^"\n" );
                                                                                (match value with
                                                                                   String str -> let strArr = Array.to_list str in
                                                                                                (match strArr with
                                                                                                | []     -> raise (DebugVal "variable name should not be empty")
                                                                                                | (x::xs) ->  print_string ("Added variable "^(string_of_eidos_val value)^" to the environment with value = "^(string_of_eidos_val value1)^"\n" );
                                                                                                             (Env.add x value1 env, Void))
                                                                                  |_           -> (new_env, value))
                                                                                )
*)
and interpAssignExpr env (Assign (cond, cond_exp_opt) : assign_expr) = 
                                 match cond_exp_opt with
                                        None -> let (new_env, value) =interpConditionalExpr env cond in
                                                (new_env,value)
                                       |Some cond1 -> let (new_env, value) =interpConditionalExpr Env.empty cond in
                                                        let (env1, value1) = interpConditionalExpr env cond1 in
                                                        (*print_string ("left hand side: "^(string_of_eidos_val value)^"\n" );
                                                        print_string ("right hand side: "^(string_of_eidos_val value1)^"\n" );*)
                                                        (match value with
                                                               String str -> let strArr = Array.to_list str in
                                                                      (match strArr with
                                                                             | []      -> raise (DebugVal "variable name should not be empty")
                                                                             | (x::xs) -> print_string ("Added variable "^(string_of_eidos_val value)^" to the environment with value = "^(string_of_eidos_val value1)^"\n" );
                                                                             (Env.add x value1 env, Void)
                                                                      )
                                                               |_           -> (new_env, value)
                                                        )
                                                           
                                                                                                                              
(* interpConditionalExpr : env -> conditional_expr -> env*eidosValue *)
and interpConditionalExpr env (Cond (lorexp, twoconds_opt) : conditional_expr )= match twoconds_opt with
                                        |None                    -> let (new_env, value) = interpLorExpr env lorexp in
                                                                    (*print_string ((string_of_eidos_val value)^"\n" );*)
                                                                    (new_env,value)
                                        |Some (cond_e1, cond_e2) ->
                                                             let (new_env, value) = interpLorExpr env lorexp in
                                                             (match value with
                                                             |Logical boolean -> if boolean == Array.of_list [true] then
                                                                                     interpConditionalExpr new_env cond_e1 else
                                                                                     interpConditionalExpr new_env cond_e2

                                                             | _ -> raise (TernaryExcept "ternary operator expects a boolean as first argument! "))


(* interpLorExpr : env -> l_or_xpr -> env*eidosValue *)
and interpLorExpr env (Lor l : l_or_expr) = match l with
                                   | []      -> (env, Void)
                                   | (x::xs) ->
                                             let (new_env, value) = interpLAndExpr env x in
                                               let (env1, value1) = interpLorExpr new_env (Lor xs) in
                                               (match (value, value1) with
                                                   |(_,Void)                            -> (env1, value) (*to pass the first value upwards*)
                                                   |(Logical boolean, Logical boolean2) -> (env1, Logical (Array.map2 (||) boolean boolean2))
                                                   |(_, _)                              -> raise (BoolEvalExcept "OR can only be called on logical values!"))


(* interpLAndExpr : env -> l_and_expr -> env*eidosValue *)
and interpLAndExpr env (Land l : l_and_expr) = match l with
                                   | []      -> (env, Void)
                                   | (x::xs) ->
                                             let (new_env, value) = interpEqtExpr env x in
                                              let (env1, value1) = interpLAndExpr new_env (Land xs) in
                                              (match (value, value1) with
                                                   |(_,Void)                            -> (env1, value) (*to pass the first value upwards*)
                                                   |(Logical boolean, Logical boolean2) -> (env1, Logical (Array.map2 (&&) boolean boolean2))
                                                   |(_, _)                              -> raise (BoolEvalExcept "AND can only be called on logical values!"))

(* interpEqtExpr : env -> eqt_expr -> env*eidosValue *)
and interpEqtExpr env (Eqt (relexpr, eqneqe_l)) = match eqneqe_l with
                                    | []      -> interpRelExpr env relexpr
                                    | (e::es) -> (match e with
                                            | (Neq rel) ->
                                                       let (new_env, value) = interpRelExpr env relexpr in
                                                        let (env1, value1) = interpEqtExpr new_env (Eqt (rel, es)) in
                                                        (env1, Logical (Array.of_list [(not (value = value1))]))
                                            | (Eq rel) -> let (new_env, value) = interpRelExpr env relexpr in
                                                            let (env1, value1) = interpEqtExpr new_env (Eqt (rel, es)) in
                                                             (env1, Logical (Array.of_list [(value = value1)])))


(* interpRelExpr : env -> rel_xpr -> env*eidosValue *)
and interpRelExpr env (Rel (add_e, compl_opt) : rel_expr) = match compl_opt with
                                              | None         -> interpAddExpr env add_e    (* this option list stuff doesn't make sense! [] is really same as None! *)
                                              | Some []      -> interpAddExpr env add_e
                                              | Some (c::cs) -> let interp_add_with op a_expr =
                                                                              let (new_env, value) = interpAddExpr env add_e in
                                                                              let (env1, value1)   = interpRelExpr new_env (Rel (a_expr, Some cs)) in
                                                                              (env1, Logical (Array.of_list [op value value1])) in (match c with
                                                             | Less addexpr  -> interp_add_with (<) addexpr
                                                             | Leq addexpr   -> interp_add_with (<=) addexpr
                                                             | Great addexpr -> interp_add_with (>) addexpr
                                                             | Geq addexpr   -> interp_add_with (>=) addexpr
                                                             )
(* interpAddExpr : env -> add_expr -> env*eidosValue *)
and interpAddExpr env (Add (mult_e, addsubmulL_opt) : add_expr )= match addsubmulL_opt with
                                                | None         -> let (env, value) = interpMultExpr env mult_e in(*throw away Plus? probably an error!*)
                                                                        (*print_string ((string_of_eidos_val value)^"\n" );*)
                                                                        (env,value)
                                                | Some []       -> interpMultExpr env mult_e
                                                | Some (a::aas)   -> let interp_mult_with op1 op2 m_expr =
                                                                                 let (new_env, value) = interpMultExpr env mult_e in
                                                                                 let (env1, value1) = interpAddExpr new_env (Add (m_expr, Some aas)) in  (* type promotion stuff *)

                                                                                  (match (value, value1) with
                                                                                          |(Integer narray, Integer marray) -> (match (Array.to_list narray, Array.to_list marray) with
                                                                                                                                    | ([], [])          -> (env1, Integer [||])
                                                                                                                                    | ([x], [])         -> (env1, Integer [|x|])
                                                                                                                                    | ([], [x])         -> (env1, Integer [|x|])
                                                                                                                                    | (x::xs as l, [n]) -> (env1, Integer (Array.map (op1 n) narray))
                                                                                                                                    | ([n], x::xs)      -> (env1, Integer (Array.map (op1 n) marray))
                                                                                                                                    | (l, l1)           -> if Array.length narray == Array.length marray
                                                                                                                                                           then (env1, Integer (Array.map2 (op1) narray marray))
                                                                                                                                                              else raise (DebugVal "vector length mismatch!"))
                                                                                          |(Float narray, Float marray)     -> (match (Array.to_list narray, Array.to_list marray) with
                                                                                                                                    | ([], [])          -> (env1, Float [||])
                                                                                                                                    | ([x], [])         -> (env1, Float [|x|])
                                                                                                                                    | ([], [x])         -> (env1, Float [|x|])
                                                                                                                                    | (x::xs as l, [n]) -> (env1, Float (Array.map (op2 n) narray))
                                                                                                                                    | ([n], x::xs)      -> (env1, Float (Array.map (op2 n) marray))
                                                                                                                                    | (l, l1)           -> if Array.length narray == Array.length marray
                                                                                                                                                           then (env1, Float (Array.map2 (op2) narray marray))
                                                                                                                                                              else raise (DebugVal "vector length mismatch!"))
                                                                                          |(Integer narray, Float marray)   -> (match (Array.to_list narray, Array.to_list marray) with
                                                                                                                                    | ([], [])          -> (env1, Float [||])
                                                                                                                                    | ([x], [])         -> (env1, Float [|float_of_int x|])
                                                                                                                                    | ([], [x])         -> (env1, Float [|x|])
                                                                                                                                    | (x::xs as l, [n]) -> (env1, Float (Array.map (fun t -> op2 (float_of_int t) n) narray))
                                                                                                                                    | ([n], x::xs)      -> (env1, Float (Array.map (fun t -> op2 (float_of_int n) t) marray))
                                                                                                                                    | (l, l1)           -> if Array.length narray == Array.length marray
                                                                                                                                                           then (env1, Float (Array.map2 (fun t t1 -> op2 (float_of_int t) t1) narray marray))
                                                                                                                                                              else raise (DebugVal "vector length mismatch!"))
                                                                                          |(Float narray, Integer marray)   -> (match (Array.to_list narray, Array.to_list marray) with
                                                                                                                                    | ([], [])          -> (env1, Float [||])
                                                                                                                                    | ([x], [])         -> (env1, Float [|x|])
                                                                                                                                    | ([], [x])         -> (env1, Float [|float_of_int x|])
                                                                                                                                    | (x::xs as l, [n]) -> (env1, Float (Array.map (fun t -> op2 (float_of_int n) t) narray))
                                                                                                                                    | ([n], x::xs)      -> (env1, Float (Array.map (fun t -> op2 (float_of_int t) n) marray))
                                                                                                                                    | (l, l1)           -> if Array.length narray == Array.length marray
                                                                                                                                                           then (env1, Float (Array.map2 (fun t t1 -> op2 (float_of_int t1) t) narray marray))
                                                                                                                                                              else raise (DebugVal "vector length mismatch!"))
                                                                                          |(String narray, Integer marray)  -> raise Undefined
                                                                                          |(v1, v2) -> print_string((string_of_eidos_val value)^ " " ^ (string_of_eidos_val value1)^ "\n");
                                                                                                     raise (TyExcept ("incompatible add!" ^ (string_of_eidos_val v1) ^ " " ^ (string_of_eidos_val v2)))) in
                                                                  (match a with
                                                                    | Plus mul  -> interp_mult_with (+) (+.) mul
                                                                    | Minus mul -> interp_mult_with (-) (-.) mul)

(* interpAddSubMulExpr : env -> add_sub_mul_expr -> env*eidosValue *)
(*and interpAddSubMulExpr = raise (DebugVal "I fail Addsubmul!")*)

(* interpMultExpr : env -> mult_expr -> env*eidosValue *)
and interpMultExpr env (Mult (seq_e, seqe_opt) : mult_expr) =
        let (env, value) = interpSeqExpr env seq_e in
                match seqe_opt with
                          None -> (env,value)
                        | Some [] -> (env,value)
                        | Some (sehead::setail) ->
                                let interp_seq_with op1 op2 seq_expr =
                                        let (env1, value1) = interpMultExpr env (Mult (seq_expr, Some setail)) in
                                                (match (value,value1) with
                                                        (Integer narray, Integer marray) -> (env1, Integer (Array.map2 (op1) narray marray))
                                                      | (Float narray, Float marray)     -> (env1, Float (Array.map2 (op2) narray marray))
                                                      | (Integer narray, Float marray)   -> (env1, Float (Array.map2 (fun x y -> op2 (float_of_int x) y ) narray marray))
                                                      | (Float narray, Integer marray)   -> (env1, Float (Array.map2 (fun x y -> op2 x (float_of_int y) ) narray marray))
                                                      | (_, _) -> raise (TyExcept "Incompatible Mult!")
                                                ) in
                                                    (match sehead with
                                                          Times seq  -> interp_seq_with ( * )  ( *.) seq
                                                        | Div seq -> interp_seq_with (/) (/.) seq
                                                        | Mod seq -> interp_seq_with (mod) (mod_float) seq
                                                    )



(* interpSeqExpr : env -> seq_expr -> env*eidosValue *)
and interpSeqExpr env (Seq (exp, exp_opt) : seq_expr) = match exp_opt with
                                                | None      -> interpExpExpr env exp
                                                | Some exp2 ->
                                                           let (new_env, value) = interpExpExpr env exp in
                                                             let (env1, value1) = interpExpExpr env exp2 in
                                                             (env1, make_seq_from_num_array value value1)

(* interpExpExpr : env -> exp_expr -> env*eidosValue *)
and interpExpExpr env (Eexpr (unary_e, unarye_opt) : exp_expr ) = interpUnaryExpr env unary_e

(* interpUnaryExpr : env -> unary_expr -> env*eidosValue *)
and interpUnaryExpr env (Post (post_e) : unary_expr ) = interpPostFixExpr env post_e

(* interpPostFixExpr : env -> postfix_expr -> env*eidosValue *)
and interpPostFixExpr env (PE (prim_e, postfix_opt) : postfix_expr ) = interpPrimaryExpr env prim_e

(* interpFuncExec : env -> function_execution -> env*eidosValue *)
(*and interpFuncExec = raise (DebugVal "I fail interpFuncExec!")*)

(* interpFuncDefn : env -> function_definition -> env*eidosValue *)
(*and interpFuncDefn = raise (DebugVal "I fail interpFuncDef!")*)

(* interpAttributeAccessor : env -> attribute_accessor -> env*eidosValue *)
(*and interpAttributeAccessor = raise (DebugVal "I fail interpAA!")*)

(* interpIndexing : env -> indexing -> env*eidosValue *)
(*and interpIndexing = raise (DebugVal "I fail interpIndexing!")*)

 (* interpPrimaryExpr : env -> primary_expr -> env*eidosValue *)
and interpPrimaryExpr env (prim_e : primary_expr) = match prim_e with
                                                        Const c -> interpConstant env c
                                                      | Ident i -> interpIdentifier env i
                                                      | E e -> interpExpr env e

(* interpConstant : env -> constant -> env*eidosValue *)
and interpConstant env c = match c with
                      | ConstInt n -> (*print_string ("Integer value: "^string_of_int n^"\n");*)
                                      (env, Integer (Array.of_list [n]))
                      | ConstFloat f -> (env, Float (Array.of_list [f]))
                      | ConstStr str -> (env, String (Array.of_list [str]))


(* interpIdentifier : env -> identifier -> env*eidosValue *)
and interpIdentifier env (id : string) = match id with
                         | var_name -> match Env.find_opt var_name env with
                               | None -> (*print_string ("Variable name: "^var_name^"\n");*)
                                         (env, String([|var_name|])) (*if the value is not found then it's either an assignment or an undefined var *)
                               | Some v -> (env, v)
(*
(* interpArgumentExpr : env -> argument_expr -> env*eidosValue *)
and interpArgumentExpr = raise Undefined

(* interpFuncDecl : env -> func_decl -> env*eidosValue *)
and interpFuncDecl = raise Undefined

(* interpReturnTypeSpec : env -> return_type_spec -> env*eidosValue *)
and interpReturnTypeSpec = raise Undefined

(* interpTypeSpec : env -> type_spec -> env*eidosValue *)
and interpTypeSpec = raise Undefined

(* interpTypeSpecH : env -> type_spec_h -> env*eidosValue *)
and interpTypeSpecH = raise Undefined

(* interpObjectClassSpec : env -> object_class_spec -> env*eidosValue *)
and interpObjectClassSpec = raise Undefined

(* interpParamList : env -> param_list -> env*eidosValue *)
and interpParamList = raise Undefined

(* interpParamSpec : env -> param_spec -> env*eidosValue *)
and interpParamSpec = raise Undefined
*)
(* function to start interpreting *)
let interp = interpBlock empty_env

(******************************************)
let get_prog () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-interpret]\n");
          exit 1) in
  let ch = open_in argv.(1) in
  Parse.interpreter_block Lex.lexer (Lexing.from_channel ch)

let _ =
        let prog = get_prog () in
        (*if prog then (print_string "Parse Success") else (print_string "Parse Error")*)
        (*print_string "Success"*)
        interp prog
