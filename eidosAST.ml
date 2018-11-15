type interp_block = Empty
                  | StmtInterp of statement * interp_block
                  | FuncInterp of fun_decl * interp_block


and statement  = Cstmt of compound_stmt
                | Expr of expr_stmt
                | Slct of select_stmt
                | For of for_stmt
                | Do of do_while_stmt
                | While of while_stmt
                | Jump of jump_stmt

and compound_stmt = CmpdStmt of (statement list)

and expr_stmt = Estmt of (assign_expr option)

and select_stmt = If of expr * statement * (statement option)

and for_stmt = ForStmt of string * expr * statement

and do_while_stmt = DoWhile of statement * expr

and while_stmt    = WhileStmt of expr * statement

and jump_stmt     = Next
                   | Break
                   | Return of (expr option)


and expr = E of conditional_expr

and assign_expr = Assign of conditional_expr * (conditional_expr option)

and conditional_expr = Cond of l_or_expr * (conditional_expr * conditional_expr) option

and l_or_expr = Lor of l_and_expr * l_and_expr list

and l_and_expr = Land of eqt_expr * eqt_expr list

and eqt_expr   = Eqt of rel_expr * rel_expr list

and rel_expr   = Rel of add_expr * add_expr list

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

and function_definition = FuncDef of primary_expr * (arg_expr list)

and attribute_accessor = AttAcc of primary_expr * identifier

and indexing = Idx of primary_expr * (conditional_expr list) option

and constant = ConstInt of int
              | ConstFloat of float
              | ConstStr of string

and primary_expr = Ident of string | Const of constant | E of expr

and arg_expr = C of conditional_expr | ArgSc of string * conditional_expr

and fun_decl = Func of return_type_spec * string * param_list * compound_stmt

and return_type_spec = RTySpec of type_spec

and type_spec = T of type_spec_h * dollar option

and dollar = Dollar

and type_spec_h = Void | Null | Logical | Float | Integer | String
                 | Obj of object_class_spec option
                 | Numeric
                 | PlusTy
                 | TimesTy
                 | TyAbrev of tyabrev list

and tyabrev = V | N | L | I | F | S | O | Ocl of object_class_spec option

and object_class_spec = OSpec of string

and param_list = Void
                | Pspec of param_spec * param_spec list

and param_spec = PSpec of type_spec * string
                | PTySpec of type_spec * string * ((constant, string) either)
