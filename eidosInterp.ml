open EidosAST
open EidosTypes

exception Undefined
exception UnboundVar of string
exception IfExpr of string
exception ForExcept of string
exception WhileExcept of string

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
                               |Estmt None     -> (env, Void)
                               |Estmt ass_expr -> interpAssignExpr ass_expr

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
and interpAssignExpr = raise Undefined

(* interpConditionalExpr : env -> conditional_expr -> env*eidosValue *)
and interpConditionalExpr = raise Undefined

(* interpLorExpr : env -> l_or_xpr -> env*eidosValue *)
and interpLorExpr = raise Undefined

(* interpLAndExpr : env -> l_and_expr -> env*eidosValue *)
and interpLAndExpr = raise Undefined

(* interpEqtExpr : env -> eqt_expr -> env*eidosValue *)
and interpEqtExpr = raise Undefined

(* interpEqNeqExpr : env -> eq_neq_expr -> env*eidosValue *)
and interpEqNeqExpr = raise Undefined

(* interpRelExpr : env -> rel_xpr -> env*eidosValue *)
and interpRelExpr = raise Undefined

(* interpComparisonExpr : env -> comparison_expr -> env*eidosValue *)
and interpComparisonExpr = raise Undefined

(* interpAddExpr : env -> add_expr -> env*eidosValue *)
and interpAddExpr = raise Undefined

(* interpAddSubMulExpr : env -> add_sub_mul_expr -> env*eidosValue *)
and interpAddSubMulExpr = raise Undefined

(* interpMultExpr : env -> mult_expr -> env*eidosValue *)
and interpMultExpr = raise Undefined

(* interpMulDivSeqExpr : env -> mul_div_seq -> env*eidosValue *)
and interpMulDivSeqExpr = raise Undefined

(* interpSeqExpr : env -> seq_expr -> env*eidosValue *)
and interpSeqExpr = raise Undefined

(* interpExpExpr : env -> exp_expr -> env*eidosValue *)
and interpExpExpr = raise Undefined

(* interpUnaryExpr : env -> unary_expr -> env*eidosValue *)
and interpUnaryExpr = raise Undefined

(* interpPostFixExpr : env -> postfix_expr -> env*eidosValue *)
and interpPostFixExpr = raise Undefined

(* interpFuncExec : env -> function_execution -> env*eidosValue *)
and interpFuncExec = raise Undefined

(* interpFuncDefn : env -> function_definition -> env*eidosValue *)
and interpFuncDefn = raise Undefined

(* interpAttributeAccessor : env -> attribute_accessor -> env*eidosValue *)
and interpAttributeAccessor = raise Undefined

(* interpIndexing : env -> indexing -> env*eidosValue *)
and interpIndexing = raise Undefined

(* interpConstant : env -> constant -> env*eidosValue *)
and interpConstant env  c = match c with
                      | ConstInt n -> (env, Integer (Array.of_list [n]))
                      | ConstFloat f -> (env, Float (Array.of_list [f]))
                      | ConstStr str -> (env, String (Array.of_list [str]))


(* interpIdentifier : env -> identifier -> env*eidosValue *)
and interpIdentifier env (id : identifier) = match id with
                         | Ident var_name -> match Env.find_opt var_name env with
                               | None -> (env, Void) (* probably better to raise an exception,
                                                     but then complicates interpretting other things such as for loop! *)
                               | Some v -> (env, v)

 (* interpPrimaryExpr : env -> primary_expr -> env*eidosValue *)
and interpPrimaryExpr = raise Undefined

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

(* function to start interpreting *)
let interp = interpBlock empty_env
