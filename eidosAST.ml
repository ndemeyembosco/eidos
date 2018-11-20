type interp_block = Empty
                  | StmtInterp of statement * interp_block
                  | FuncInterp of func_decl * interp_block


and statement  = Cstmt of compound_stmt
                | ExprStmt of expr_stmt
                | SlctStmt of select_stmt
                | For of for_stmt
                | Do of do_while_stmt
                | While of while_stmt
                | Jump of jump_stmt

and compound_stmt = CmpdStmt of (statement list)

and expr_stmt = Estmt of (assign_expr option)

and select_stmt = If of expr * compound_stmt * (compound_stmt option)

and for_stmt = ForStmt of string * expr * statement

and do_while_stmt = DoWhile of statement * expr

and while_stmt    = WhileStmt of expr * statement

and jump_stmt     = Next
                   | Break
                   | Return of (expr option)


and expr = E of conditional_expr

and assign_expr = Assign of conditional_expr * (conditional_expr option)

and conditional_expr = Cond of l_or_expr * (conditional_expr * conditional_expr) option

and l_or_expr = Lor of l_and_expr list

and l_and_expr = Land of eqt_expr list

and eqt_expr   = Eqt of rel_expr * (eq_neq_expr list)

and eq_neq_expr = Neq of rel_expr | Eq of rel_expr

and rel_expr   = Rel of add_expr * (comparison_expr list) option

and comparison_expr = Less of add_expr | Leq of add_expr | Great of add_expr | Geq of add_expr

and add_expr  = Add of mult_expr * (add_sub_mul list) option

and add_sub_mul = Plus of mult_expr | Minus of mult_expr

and mult_expr    = Mult of seq_expr * (mul_div_seq list) option

and mul_div_seq = Times of seq_expr | Div of seq_expr | Mod of seq_expr

and seq_expr    = Seq of exp_expr * exp_expr option

and exp_expr    = Eexpr of unary_expr * exp_expr option

and unary_expr  = Post of postfix_expr
                | NegExpr of unary_expr
                | PlusExpr of unary_expr
                | ExclaimExpr of unary_expr

and postfix_expr  = PE of primary_expr
                  | FE of function_execution
                  | FD of function_definition
                  | AA of attribute_accessor
                  | Ind of indexing

and function_execution = primary_expr

and function_definition = FuncDef of primary_expr * (argument_expr list)

and attribute_accessor = AttAcc of primary_expr * string

and indexing = Idx of primary_expr * (conditional_expr list) option

and primary_expr = Ident of string | Const of constant | E of expr

and constant = ConstInt of int
              | ConstFloat of float
              | ConstStr of string

and identifier = Ident of string

and argument_expr = C of conditional_expr | ArgSc of string * conditional_expr

and func_decl = Func of return_type_spec * string * param_list * compound_stmt

and return_type_spec = RTySpec of type_spec

and type_spec = T of type_spec_h | TDollar of type_spec_h

and type_spec_h = Void | Null | Logical | Float | Integer | String
                 | Obj of object_class_spec option
                 | Numeric
                 | PlusTy
                 | TimesTy
                 | TyList of type_spec_h list 


and object_class_spec = OSpec of string

and param_list = Void
                | Pspec of param_spec list

and param_spec = PSpec of type_spec * string
                | PTySpecC of type_spec * string * constant
                | PTySpecI of type_spec * string * string
