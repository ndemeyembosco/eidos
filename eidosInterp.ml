open EidosAST
open EidosTypes

exception Undefined

(* interpblock : env -> interp_block -> eidosValue *)
let rec interpBlock env = raise Undefined

(* interpStatement : env -> statement -> eidosValue *)
let rec interpStatement = raise Undefined

(* interpCompoundStmt : env -> compound_stmt -> eidosValue *)
let rec interpCompoundStmt = raise Undefined

(* interpExprStmt : env -> expr_stmt -> eidosValue *)
let rec interpExprStmt = raise Undefined

(* interpSelectStmt : env -> select_stmt -> eidosValue *)
let rec interpSelectStmt = raise Undefined

(* interpForStmt : env -> for_stmt -> eidosValue *)
let rec interpForStmt = raise Undefined

(* interpDoWhileStmt : env -> do_while_stmt -> eidosValue *)
let rec interpDoWhileStmt = raise Undefined

(* interpWhileStmt : env -> while_stmt -> eidosValue *)
let rec interpWhileStmt = raise Undefined

(* interpJumpStmt : env -> jump_stmt -> eidosValue *)
let rec interpJumpStmt = raise Undefined

(* interpExpr : env -> expr -> eidosValue *)
let rec interpExpr = raise Undefined

(* interpAssignExpr : env -> assign_expr -> eidosValue *)
let rec interpAssignExpr = raise Undefined

(* interpConditionalExpr : env -> conditional_expr -> eidosValue *)
let rec interpConditionalExpr = raise Undefined

(* interpLorExpr : env -> l_or_xpr -> eidosValue *)
let rec interpLorExpr = raise Undefined

(* interpLAndExpr : env -> l_and_expr -> eidosValue *)
let rec interpLAndExpr = raise Undefined

(* interpEqtExpr : env -> eqt_expr -> eidosValue *)
let rec interpEqtExpr = raise Undefined

(* interpEqNeqExpr : env -> eq_neq_expr -> eidosValue *)
let rec interpEqNeqExpr = raise Undefined

(* interpRelExpr : env -> rel_xpr -> eidosValue *)
let rec interpRelExpr = raise Undefined

(* interpComparisonExpr : env -> comparison_expr -> eidosValue *)
let rec interpComparisonExpr = raise Undefined

(* interpAddExpr : env -> add_expr -> eidosValue *)
let rec interpAddExpr = raise Undefined

(* interpAddSubMulExpr : env -> add_sub_mul_expr -> eidosValue *)
let rec interpAddSubMulExpr = raise Undefined

(* interpMultExpr : env -> mult_expr -> eidosValue *)
let rec interpMultExpr = raise Undefined

(* interpMulDivSeqExpr : env -> mul_div_seq -> eidosValue *)
let rec interpMulDivSeqExpr = raise Undefined

(* interpSeqExpr : env -> seq_expr -> eidosValue *)
let rec interpSeqExpr = raise Undefined

(* interpExpExpr : env -> exp_expr -> eidosValue *)
let rec interpExpExpr = raise Undefined

(* interpUnaryExpr : env -> unary_expr -> eidosValue *)
let rec interpUnaryExpr = raise Undefined

(* interpPostFixExpr : env -> postfix_expr -> eidosValue *)
let rec interpPostFixExpr = raise Undefined

(* interpFuncExec : env -> function_execution -> eidosValue *)
let rec interpFuncExec = raise Undefined

(* interpFuncDefn : env -> function_definition -> eidosValue *)
let rec interpFuncDefn = raise Undefined

(* interpAttributeAccessor : env -> attribute_accessor -> eidosValue *)
let rec interpAttributeAccessor = raise Undefined

(* interpIndexing : env -> indexing -> eidosValue *)
let rec interpIndexing = raise Undefined

(* interpConstant : env -> constant -> eidosValue *)
let rec interpConstant = raise Undefined

(* interpIdentifier : env -> identifier -> eidosValue *)
let rec interpIdentifier = raise Undefined

 (* interpPrimaryExpr : env -> primary_expr -> eidosValue *)
let rec interpPrimaryExpr = raise Undefined

(* interpArgumentExpr : env -> argument_expr -> eidosValue *)
let rec interpArgumentExpr = raise Undefined

(* interpFuncDecl : env -> func_decl -> eidosValue *)
let rec interpFuncDecl = raise Undefined

(* interpReturnTypeSpec : env -> return_type_spec -> eidosValue *)
let rec interpReturnTypeSpec = raise Undefined

(* interpTypeSpec : env -> type_spec -> eidosValue *)
let rec interpTypeSpec = raise Undefined

(* interpTypeSpecH : env -> type_spec_h -> eidosValue *)
let rec interpTypeSpecH = raise Undefined

(* interpObjectClassSpec : env -> object_class_spec -> eidosValue *)
let rec interpObjectClassSpec = raise Undefined

(* interpParamList : env -> param_list -> eidosValue *)
let rec interpParamList = raise Undefined

(* interpParamSpec : env -> param_spec -> eidosValue *)
let rec interpParamSpec = raise Undefined

(* function to start interpreting *)
let interp = interpBlock empty_env
