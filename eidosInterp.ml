open EidosAST
open EidosTypes
(* open Base.Float *)
exception Undefined
exception UnboundVar of string
exception IfExpr of string
exception ForExcept of string
exception WhileExcept of string
exception TernaryExcept of string
exception BoolEvalExcept of string
exception DebugVal of string


(* let neg (f : float) : float = -f  *)
(* interpblock : env -> interp_block -> env*eidosValue*)  (* return last value of the block*)
let rec interpBlock env (int_b : interp_block) = match int_b with
                      | Empty      -> (env, Void)
                      | StmtInterp(stmt, int_b1) ->
                                                 let (new_env, _) = interpStatement env stmt in
                                                    interpBlock new_env int_b1
                      | FuncInterp(func_decl, int_b1) ->
                                                  let (new_env, _) = interpFuncDecl env func_decl in
                                                    interpBlock new_env int_b1


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
                         |ForStmt (str, expr, stmt) ->  let rec loop e =
                                                             let (new_env, value) = interpExpr e expr in (* probably a try catch to recover from errors! *)
                                                             (match value with
                                                              | Void -> (new_env, Void)
                                                              | _    -> let (env1, _) = interpStatement (Env.add str value e) stmt in
                                                                        loop env1) in
                                                               loop env


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
and interpAssignExpr env (ass : assign_expr) = (* This implementation is wrong! just an attempt to get sth working : *)
                                 match ass with
                                 | Assign (cond, cond_exp_opt) -> let (new_env, value) = interpConditionalExpr env cond in (* value is string! *)
                                                                        (match cond_exp_opt with
                                                                        | None       -> (match value with
                                                                              String str -> let strArr = Array.to_list str in
                                                                                              (match strArr with
                                                                                              | []     -> raise (DebugVal "variable name should not be empty")
                                                                                              | (x::xs) -> (Env.add x Void env, Void))
                                                                              | v          -> (new_env, v))
                                                                              (* raise (DebugVal "varible should be string!" *)

                                                                        | Some cond1 ->
                                                                                let (env1, value1) = interpConditionalExpr new_env cond1 in
                                                                                (match value with
                                                                                   String str -> let strArr = Array.to_list str in
                                                                                                (match strArr with
                                                                                                | []     -> raise (DebugVal "variable name should not be empty")
                                                                                                | (x::xs) -> (Env.add x value1 env, Void))
                                                                                  |v           -> (env1, v)
                                                                                  (* raise (DebugVal "varible should be string!")) *)
                                                                                ))



(* interpConditionalExpr : env -> conditional_expr -> env*eidosValue *)
and interpConditionalExpr env (Cond (lorexp, twoconds_opt) : conditional_expr )= match twoconds_opt with
                                        |None                    -> interpLorExpr env lorexp
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
                                                   |(Logical boolean, Logical boolean2) -> (env1, Logical (Array.map2 (||) boolean boolean2))
                                                   |(_, _)                              -> (env1, value1))
                                                   (* raise (BoolEvalExcept (string_of_eidos_val value ^ " " ^ (string_of_eidos_val value1)))) *)


(* interpLAndExpr : env -> l_and_expr -> env*eidosValue *)
and interpLAndExpr env (Land l : l_and_expr) = match l with
                                   | []      -> raise (DebugVal "failed on and!")
                                   (* (env, Void) *)
                                   | (x::xs) ->
                                             let (new_env, value) = interpEqtExpr env x in
                                              let (env1, value1) = interpLAndExpr new_env (Land xs) in
                                              (match (value, value1) with
                                                   |(Logical boolean, Logical boolean2) -> (env1, Logical (Array.map2 (&&) boolean boolean2))
                                                   |(_, _)                              -> (env1, value)) (* not the right  thing to do,but for now!*)
                                                   (* raise (BoolEvalExcept (string_of_eidos_val value1 ^ " " ^ (string_of_eidos_val value)))) *)

(* interpEqtExpr : env -> eqt_expr -> env*eidosValue *)
and interpEqtExpr env (Eqt (relexpr, eqneqe_l)) = match eqneqe_l with
                                    | []      -> interpRelExpr env relexpr
                                    | (e::es) -> (match e with
                                            | (Neq rel) ->
                                                       let (new_env, value) = interpRelExpr env relexpr in
                                                        let (env1, value1) = interpEqtExpr new_env (Eqt (rel, es)) in
                                                        (env1, Logical (Array.of_list [(not (value == value1))]))
                                            | (Eq rel) -> let (new_env, value) = interpRelExpr env relexpr in
                                                            let (env1, value1) = interpEqtExpr new_env (Eqt (rel, es)) in
                                                             (env1, Logical (Array.of_list [(value == value1)])))


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
                                                | None         -> interpMultExpr env mult_e (*throw away Plus? probably an error!*)
                                                | Some []       -> interpMultExpr env mult_e
                                                | Some (a::aas)   -> let interp_mult_with op1 op2 m_expr =
                                                                                 let (new_env, value) = interpMultExpr env mult_e in
                                                                                 let (env1, value1) = interpAddExpr new_env (Add (m_expr, Some aas)) in  (* type promotion stuff *)
                                                                                  (match (value, value1) with
                                                                                          |(Integer narray, Integer marray) -> (env1, Integer (Array.map2 (op1) narray marray))
                                                                                          |(Float narray, Float marray)     -> (env1, Float (Array.map2 (op2) narray marray))
                                                                                          |(Integer narray, Float marray)   -> (env1, Float (Array.map2 (fun x y -> op2 (float_of_int x) y ) narray marray))
                                                                                          |(Float narray, Integer marray)   -> (env1, Float (Array.map2 (fun x y -> op2 x (float_of_int y) ) narray marray))
                                                                                          |(_, _) -> raise (TyExcept "incompatible add!")) in
                                                                  (match a with
                                                                    | Plus mul  -> interp_mult_with (+) (+.) mul
                                                                    | Minus mul -> interp_mult_with (-) (-.) mul)

(* interpMultExpr : env -> mult_expr -> env*eidosValue *)
and interpMultExpr env (Mult (seq, muldiv_opt))= match muldiv_opt with
                                    None    -> interpSeqExpr env seq
                                  | Some [] -> interpSeqExpr env seq
                                  | Some (x::xs) ->
                                                let interp_seq_expr_with op1 op2 m_expr =
                                                   let (new_env, value) = interpSeqExpr env seq in
                                                   let (env1, value1) = interpMultExpr new_env (Mult (m_expr, Some xs)) in
                                                   (match (value, value1) with
                                                           |(Integer narray, Integer marray) -> (env1, Integer (Array.map2 (op1) narray marray))
                                                           |(Float narray, Float marray)     -> (env1, Float (Array.map2 (op2) narray marray))
                                                           |(Integer narray, Float marray)   -> (env1, Float (Array.map2 (fun x y -> op2 (float_of_int x) y ) narray marray))
                                                           |(Float narray, Integer marray)   -> (env1, Float (Array.map2 (fun x y -> op2 x (float_of_int y) ) narray marray))
                                                           |(_, _) -> raise (TyExcept "incompatible add!")) in
                                   (match x with
                                     | Times mul  -> interp_seq_expr_with (fun x y -> x * y) (fun x y -> x *. y) mul
                                     | Div mul    -> interp_seq_expr_with (/) (/.) mul
                                     | Mod mul    -> raise Undefined)


(* interpSeqExpr : env -> seq_expr -> env*eidosValue *)
and interpSeqExpr env (Seq (exp, exp_opt) : seq_expr) = match exp_opt with
                                                | None      -> interpExpExpr env exp
                                                | Some exp2 ->
                                                           let (new_env, value) = interpExpExpr env exp in
                                                             let (env1, value1) = interpExpExpr env exp2 in
                                                             (env1, make_seq_from_num_array value value1)


(* interpExpExpr : env -> exp_expr -> env*eidosValue *)
and interpExpExpr env (Eexpr (un, exp_opt) : exp_expr) = match exp_opt with
                                                | None     -> interpUnaryExpr env un
                                                | Some exp ->
                                                           let (new_env, value) = interpUnaryExpr env un in
                                                            let (env1, value1) = interpExpExpr new_env exp in
                                                            (* raise value to value1 exponent! *)
                                                            match (value, value1) with
                                                            | (Integer l, Integer l2) -> (env1, Integer (Array.map2 pow l l2))
                                                            | (Float l, Float l2)     -> (env1, Float (Array.map2 (fun x y -> x ** y) l l2))
                                                            | (Integer l, Float l2)   -> (env1, Float (Array.map2 (fun x y -> (Int32.to_float(Int32.of_int x)) ** y) l l2))
                                                            | (Float l, Integer l2)   -> (env1, Float (Array.map2 (fun x y -> x ** (Int32.to_float(Int32.of_int y))) l l2))
                                                            | (_, _)                  -> raise (DebugVal "exponent should be called only on integers and floats!")



(* interpUnaryExpr : env -> unary_expr -> env*eidosValue *)
and interpUnaryExpr env un = match un with
                    | Post post   -> interpPostFixExpr env post
                    | NegExpr un1 ->
                                  let (new_env, value) = interpUnaryExpr env un1
                                  in (match value with
                                        | Integer l -> (new_env, Integer (Array.map (fun x -> -x) l))
                                        (* | Float l -> (new_env, Float (Array.map (fun x -> neg x) l)) *)
                                        | _       -> raise (DebugVal "can only negate integers or floats!"))
                    | PlusExpr un1    -> interpUnaryExpr env un1 (* absolutely wrong! *)
                    | ExclaimExpr exp -> raise Undefined (* Not sure what this is supposed to be *)



(* interpPostFixExpr : env -> postfix_expr -> env*eidosValue *)
and interpPostFixExpr env (post : postfix_expr) = match post with
                        | PE pr          ->
                        (* raise (DebugVal "I fail here!") *)
                                 interpPrimaryExpr env pr
                        | FE fn_exec     -> interpFuncExec env fn_exec
                        | FD fn_def      -> interpFuncDefn env fn_def
                        | AA attr_access -> interpAttributeAccessor env attr_access
                        | Ind ind        -> interpIndexing env ind

(* interpFuncExec : env -> function_execution -> env*eidosValue *)
and interpFuncExec env fn = interpPrimaryExpr env fn
       (* raise (DebugVal "failed in function execution") *)

(* interpFuncDefn : env -> function_definition -> env*eidosValue *)
and interpFuncDefn env _ = (env, Void)
    (* raise (DebugVal "failed in function definition") *)

(* interpAttributeAccessor : env -> attribute_accessor -> env*eidosValue *)
and interpAttributeAccessor env _ = (env, Void)

(* interpIndexing : env -> indexing -> env*eidosValue *)
and interpIndexing env (Idx (pr, cond_l_opt) : indexing) = (env, Void)


(* interpConstant : env -> constant -> env*eidosValue *)
and interpConstant env  c = match c with
                      | ConstInt n ->
                      (* raise (DebugVal ("failing on int const!" ^ string_of_int n)) *)
                      (env, Integer (Array.of_list [n]))
                      | ConstFloat f -> (env, Float (Array.of_list [f]))
                      | ConstStr str -> (env, String (Array.of_list [str]))


(* interpIdentifier : env -> identifier -> env*eidosValue *)
and interpIdentifier env (id : identifier) = match id with
                         | Ident var_name -> match Env.find_opt var_name env with
                               | None -> (env, Void) (* probably better to raise an exception,
                                                     but then complicates interpretting other things such as for loop! *)
                               | Some v -> (env, v)

 (* interpPrimaryExpr : env -> primary_expr -> env*eidosValue *)
and interpPrimaryExpr env pr = match pr with
                       Id str -> (match Env.find_opt str env with
                             | None -> (env, Void) (* probably better to raise an exception,
                                                   but then complicates interpretting other things such as for loop! *)
                             | Some v -> (env, v))
                      | Const c  ->
                      (* raise (DebugVal "I fail here on const!") *)
                         interpConstant env c
                      | Ex expr   -> interpExpr env expr

(* interpArgumentExpr : env -> argument_expr -> env*eidosValue *)
and interpArgumentExpr env _ = (env, Void)

(* interpFuncDecl : env -> func_decl -> env*eidosValue *)
and interpFuncDecl env _ = (env, Void)

(* interpReturnTypeSpec : env -> return_type_spec -> env*eidosValue *)
and interpReturnTypeSpec env _ = (env, Void)

(* interpTypeSpec : env -> type_spec -> env*eidosValue *)
and interpTypeSpec env _ = (env, Void)

(* interpTypeSpecH : env -> type_spec_h -> env*eidosValue *)
and interpTypeSpecH env _ = (env, Void)

(* interpObjectClassSpec : env -> object_class_spec -> env*eidosValue *)
and interpObjectClassSpec env _ = (env, Void)

(* interpParamList : env -> param_list -> env*eidosValue *)
and interpParamList env _ = (env, Void)

(* interpParamSpec : env -> param_spec -> env*eidosValue *)
and interpParamSpec env _ = (env, Void)

(* function to start interpreting *)
let interp = interpBlock empty_env
