open EidosAST
open EidosTypes

exception Undefined
exception UnboundVar of string

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
                                    | Cstmt cmpd_stmt -> interpCompoundStmt cmpd_stmt
                                    | ExprStmt exp_stmt -> interpExprStmt exp_stmt
                                    | For forstmt -> interpForStmt forstmt
                                    | Do do_while_stmt -> interpDoWhileStmt do_while_stmt
                                    | While whl -> interpWhileStmt whl
                                    | Jump jstmt -> interpJumpStmt jstmt
                                    | SlctStmt selec -> interpSelectStmt selec 

(* interpCompoundStmt : env -> compound_stmt -> env*eidosValue *)
and interpCompoundStmt = raise Undefined

(* interpExprStmt : env -> expr_stmt -> env*eidosValue *)
and interpExprStmt = raise Undefined

(* interpSelectStmt : env -> select_stmt -> env*eidosValue *)
and interpSelectStmt = raise Undefined

(* interpForStmt : env -> for_stmt -> env*eidosValue *)
and interpForStmt = raise Undefined

(* interpDoWhileStmt : env -> do_while_stmt -> env*eidosValue *)
and interpDoWhileStmt = raise Undefined

(* interpWhileStmt : env -> while_stmt -> env*eidosValue *)
and interpWhileStmt = raise Undefined

(* interpJumpStmt : env -> jump_stmt -> env*eidosValue *)
and interpJumpStmt = raise Undefined

(* interpExpr : env -> expr -> env*eidosValue *)
and interpExpr = raise Undefined

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
                               | None -> raise (UnboundVar var_name)
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
