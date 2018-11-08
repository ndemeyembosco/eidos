open EidosAST


(* string_of_interp_b : interp_block -> string *)
let string_of_interp_b (interp : interp_block) = match interp with
                       | I (n, EOF) -> match n with
                          | None -> "I None"
                          | Some i -> string_of_interp_list i

let string_of_interp_list l = List.map string_of_interp_b_1 l 

let string_of_interp_1 interpb = match interpb with
                        | Smt stmt -> string_of_stmt stmt
                        | FnDecl func -> string_of_func func

let string_of_stmt stmt = match stmt with
                 | Cstmt cmpst    -> string_of_cmpst cmpst
                 | Expr expr_stmt -> string_of_expr expr_stmt
                 | Slct slct      -> string_of_select slct
                 | For for_stmt   -> string_of_for for_stmt
                 | Do do_stmt     -> string_of_do do_stmt
                 | While w_stmt   -> string_of_while w_stmt
                 | Jump j_stmt    -> string_of_jump j_stmt

let string_of_cmpst cmpst = match cmpst with
               | CmpdStmt None -> "Compound None"
               | CmpdStmt (Some stmt_list) -> "Compound (Some(" ^ (String.concat "" (List.map string_of_stmt stmt_list)) ^ "))"

let string_of_expr expr_stmt = match expr_stmt with
                      | Estmt None            -> "Estmt None"
                      | Estmt (Some ass_expr) -> "Estmt (Some (" ^ string_of_ass ass_expr ^ "))"

let string_of_select ifstmt = match ifstmt with
                     | If (e, stmt, opstmt) -> match opstmt with
                                 | None       -> "If" ^ paren (string_of_e e) ^ paren (string_of_stmt stmt) ^ "None"
                                 | Some stmt1 -> "If" ^ paren (string_of_e e) ^ paren (string_of_stmt stmt) ^ "(Some (" ^ (string_of_stmt stmt1) ^ "))"
(* paren : str -> str  *)
let paren str = "(" ^ str ^ ")"

let string_of_for for_stmt = match for_stmt with
                   | ForStmt (str, exp, stmt) -> "For " ^ str ^ (paren (string_of_e e)) ^ (paren (string_of_stmt stmt))


let string_of_do do_stmt  = match do_stmt with
                   | DoWhile (stmt, ep) -> "DoWhile " ^ (paren (string_of_stmt stmt)) ^ (paren (string_of_e ep))

let string_of_while w_stmt = match w_stmt with
                    | WhileStmt (e, stmt) -> "While " ^ (paren (string_of_e e)) ^ (paren (string_of_stmt stmt))

let string_of_jump j_stmt = match j_stmt with
                   | Next  -> "Next "
                   | Break -> "Break "
                   | Return None     -> "Return None"
                   | Return (Some e) -> "Return " ^ paren (string_of_e e)



let string_of_e exp = match exp with
             | E cond -> string_of_cond cond

let string_of_ass ass_expr = match ass_expr with
                    | Assign (cond, opcond) -> match opcond with
                                   None       -> "Assign " ^ paren (string_of_cond cond) ^ "None "
                                  |Some cond1 -> "Assign " ^ paren (string_of_cond cond) ^ (paren (string_of_cond cond1))

let string_of_cond cond = match cond with
                 | Cond (l_or, opcondT) -> match opcondT with
                                 None -> "Conditional " ^ paren (string_of_l_or l_or) ^ "None"
                                | Some cond1 -> "Conditional " ^ paren (string_of_l_or l_or) ^ "Some (" ^ paren (string_of_cond cond1) ^ ")"

let string_of_l_or l_or_expr = match l_or_expr with
                      | Lor (l_and, l_and_list) -> "Lor " ^ (paren (string_of_l_and l_and)) ^ paren (String.concat "" (List.map string_of_l_and l_and_list))

let string_of_l_and l_and_expr = match l_and_expr with
                        | Land (eqt_exp, eqt_exp_list) -> "Land " ^ (paren (string_of_eqt eqt_exp)) ^ paren (String.concat "" (List.map string_of_eqt eqt_exp_list))

let string_of_eqt eqt_exp = match eqt_exp with
                   | Eqt (rel_expr, rel_expr_list) -> "Eqt " ^ (paren (string_of_rel rel_expr)) ^ paren (String.concat "" (List.map string_of_rel rel_expr_list))

let string_of_rel rel_exp = match rel_exp with
                   | Rel (add_expr, add_expr_list) -> "Rel " ^ (paren (string_of_add add_expr)) ^ paren (String.concat "" (List.map string_of_add add_expr_list))

let string_of_add add_expr = match add_expr with
                   | Add (mult_expr, opt_add_sub_mul_list) -> match opt_add_sub_mul_list with
                                                    | None                  -> "Add " ^ (paren (string_of_mult mult_expr)) ^ "None "
                                                    | Some add_sub_mul_list -> "Add " ^ (paren (string_of_mult mult_expr)) ^ "Some (" ^ (paren (String.concat "" (List.map string_of_add_sub_mul add_sub_mul_list))) ^ ")"

let string_of_add_sub_mul add_sub_mul = match add_sub_mul with
                               | Plus mult_e  -> "Plus " ^ (paren (string_of_mult mult_e))
                               | Minus mult_e -> "Minus " ^ (paren (string_of_mult mult_e))

let string_of_mult mult = match mult with
                 | Mult (seq_expr, opt_mul_div_seq_list)  -> match opt_mul_div_seq_list with
                                                   | None                  -> "Mult " ^ (paren (string_of_seq seq_expr)) ^ "None "
                                                   | Some mul_div_seq_list -> "Mult " ^ (paren (string_of_seq seq_expr)) ^ "Some (" ^ (paren (String.concat "" (List.map string_of_mul_div_seq mul_div_seq_list))) ^ ")"

let string_of_mul_div_seq = match mul_div_seq with
                   | Times seq_expr -> "Times " ^ (paren (string_of_seq seq_expr))
                   | Div seq_expr   -> "Div " ^ (paren (string_of_seq seq_expr))
                   | Mod seq_expr   -> "Mod " ^ (paren (string_of_seq seq_expr))

let string_of_seq seq_expr = match seq_expr with
                    | Seq (exp_expr, opt_exp_expr) -> match opt_exp_expr with
                                     | None           -> "Seq " ^ (paren (string_of_exp_expr exp_expr)) ^ "None "
                                     | Some exp_expr1 -> "Seq " ^ (paren (string_of_exp_expr exp_expr)) ^ "Some (" ^ paren (string_of_exp_expr exp_expr1) ^ ")"

let string_of_exp_expr exp_expr = match exp_expr with
                         | Eexpr (unary_expr, opt_exp_expr) -> match opt_exp_expr with
                                                     | None           -> "ExpExpr " ^ (paren (string_of_unary unary_expr)) ^ "None "
                                                     | Some exp_expr1 -> "ExpExpr " ^ (paren (string_of_unary unary_expr)) ^ "Some (" ^ paren (string_of_exp_expr exp_expr1) ^ "))"


let string_of_unary unary_expr = match unary_expr with
                     | Unary un_expr   -> "Unary " ^ (paren (string_of_unary un_expr))
                     | Post postf_expr -> "Post " ^ (paren (string_of_post postf_expr))

let string_of_post postf_expr = match postf_expr with
                       | Pexpr (primary_expr, opt_post_helper_list) -> match opt_post_helper_list with
                                                             | None                   -> "Pexpr " ^ (paren (string_of_primary primary_expr)) ^ "None "
                                                             | Some post_helper_list  -> "Pexpr " ^ (paren (string_of_primary primary_expr)) ^ "Some (" ^ (paren (String.concat "" (List.map string_of_post_helper post_helper_list)))

let string_of_post_helper post_helper = match post_helper with
                               | Exp (opt_exp, opt_exp_list) -> match (opt_exp, opt_exp_list) with
                                                      | (None, None)          -> "Exp None None"
                                                      | (Some exp, None)      -> "Exp (Some " ^ paren (string_of_e exp) ^ ") None"
                                                      | (None, Some exp_list) -> "Exp None " ^ paren (String.concat "" (List.map string_of_e exp_list)) ^ ")"
                               | Paren                       -> "Paren "
                               | ArgList argument_expr_list  -> "ArgList " ^ paren (String.concat "" (List.map string_of_arg argument_expr_list))
                               | Str str                     -> "Str " ^ str

let string_of_const const = match const with
                   | Left   i  -> "Int " ^ string_of_int i
                   | Right str -> "Str " ^ str

let string_of_primary primary_expr = match primary_expr with
                            | String s -> "String " ^ s
                            | Const c  -> "Const " ^ paren (string_of_const c)
                            | E expr   -> "E " ^ paren (string_of_e expr)

let string_of_arg_expr_list arglist = match arglist with
                            | Arg (arg_expr, arg_expr_list) -> "Arg " ^ paren (string_of_arg arg_expr) ^ (paren (String.concat "" (List.map string_of_arg arg_expr_list)))

let string_of_arg arg_expr = match arg_expr with
                    | C cond -> "C " ^ paren (string_of_cond cond)
                    | Scond (s, cond) -> "Scond " ^ s ^ paren (string_of_cond cond)

let string_of_func fun_decl = match fun_decl with
                     | Func (ret_ty_spec, str, prm_lst, cmpd_stmt) -> "Func " ^ (paren (string_of_return ret_ty_spec)) ^ str ^ (paren (string_of_param_list)) ^ (paren (string_of_cmpst cmpd_stmt))

let string_of_return ret_ty_spec = match ret_ty_spec with
                          | RTySpec ty_spec -> "RTySpec " ^ string_of_ty ty_spec

let string_of_ty ty_spec = match ty_spec with
                  | T (ty_s_h, opt_dollar) -> match opt_dollar with
                                    | None        -> "T " ^ paren (string_of_ty_s_h ty_s_h) ^ "None "
                                    | Some dollar -> "T " ^ paren (string_of_ty_s_h ty_s_h) ^ "Some (" ^ string_of_dollar ^ ")"

let string_of_dollar dollar = "Dollar "

let string_of_ty_s_h ty_s_h = match ty_s_h with
                     | Void    -> "Void "
                     | Null    -> "Null "
                     | Logical -> "Logical "
                     | Float   -> "Float "
                     | Integer -> "Integer "
                     | String  -> "String "
                     | Obj opt_obj_cls -> match opt_obj_cls with
                                | None     -> "Obj None"
                                | Some obj -> "Obj Some (" ^ paren (string_of_obj_cls obj) ^ ")"
                     | Numeric -> "Numeric "
                     | PlusTy  -> "PlusTy "
                     | TimesTy -> "TimesTy "
                     | TyAbrev ty_abr_lst -> "TyAbrev " ^ paren (String.concat "" (List.map string_of_ty_abrv ty_abr_lst))


let string_of_ty_abrv tyabrev = match tyabrev with
                      | V     -> "V"
                      | N     -> "N"
                      | L     -> "L"
                      | I     -> "I"
                      | F     -> "F"
                      | S     -> "S"
                      | O     -> "O"
                      | Ocl opt_obj_cls -> match opt_obj_cls with
                                 | None     ->  "Ocl None"
                                 | Some obj ->  "Ocl Some (" ^ paren (string_of_obj_cls obj) ^ ")"


let string_of_obj_cls object_class_spec = match object_class_spec with
                                | OSpec str -> "OSpec " ^ str

let string_of_param_list param_list = match param_list with
                             | Void -> "Void "
                             | Pspec(param_spec, param_spec_list) -> "Pspec " ^ (paren (string_of_param_spec param_spec)) ^ paren (String.concat "" (List.map string_of_param_spec param_spec_list))

let string_of_param_spec param_spec = match param_spec with
                             | PSpec(ty_spec, str) -> "PSpec " ^ paren (string_of_ty ty_spec) ^ str
                             | PTySpec(ty_spec, str, const_str) -> match const_str with
                                                         | Left c  -> "PTySpec " ^ paren (string_of_ty ty_spec) ^ str ^ "Left " ^ (string_of_const c)
                                                         | Right s -> "PTySpec " ^ paren (string_of_ty ty_spec) ^ str ^ "Right " ^ s
