(* all these are constructors for arrayor values in ocaml. *)
exception Nil
exception TyExcept of string
exception Undefined



(* an eidos value is a void (which we will interpret by raise Nil : 'a),
otherwise,it a vector. that means that null, logical (which is really
bool), integer, float, string, object (environment specific),
numeric (integer or float), * (which is the polymorphic type Top)
, + (which represents all other values other than * and void), or a custom/user defined objects. These are all sugar
for vectors.*)
type eidosValue =
               Void
              | Null of (eidosValue array)
              | Logical of (bool array)
              | Integer of (int array)
              | Float of (float array)
              | String of (string array)
              | VObject of (eidosValue array)
              | Num of (numeric array)
              | VPlus of (plus_op_value array)
              | VTimes of (eidosValue array)
              (* | CustomObj of (customObj array) *)

(* type representing + types  *)
and plus_op_value =
               | N of (eidosValue array)
               | L of (bool array)
               | I of (int array)
               | F of (float array)
               | S of (string array)
               | Numeric of (numeric array)
               | P of (plus_op_value array)
               | T of (eidosValue array)
               (* | O of (customObj array) *)

(* numeric is the union of int and float types *)
and numeric = Int of int | Flt of float

(* a user defined types name. This representation is almost
certainly wrong. But since it is eidos environment dependent
I believe if should be a pair of the name of the object
and its class/data type.  *)

(* and customObj = Object of (string * eidosValue) *)

(* our value environment is a mapping from variable names to their values.  *)
module Env = Map.Make(String)

type env   = eidosValue Env.t
let empty_env : env = Env.empty
let addT_env : env = Env.add "T" (Logical [|true|]) empty_env
let addF_env : env = Env.add "F" (Logical [|false|]) addT_env
let addPi_env : env = Env.add "PI" (Float [|3.14159|]) addF_env
let initial_env : env = Env.add "E" (Float [|2.71828|]) addPi_env

let apply_int_func (f : int -> int -> int) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                     | (Integer narray, Integer marray) -> Integer (Array.map2 f narray marray)
                     | _ -> raise (TyExcept "expected integers!")

let apply_int_bool_func (f : int -> int -> bool) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                     | (Integer narray, Integer marray) -> Logical (Array.map2 f narray marray)
                     | _ -> raise (TyExcept "expected integers!")

let apply_bool_func (f : bool -> bool -> bool) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                     | (Logical b1array, Logical b2array) -> Logical (Array.map2 f b1array b2array)
                     | _ -> raise (TyExcept "expected booleans")


let apply_float_func (f : float -> float -> float) (e : eidosValue) (e1 : eidosValue)
               = match (e, e1) with
                    | (Float narray, Float marray) -> Float (Array.map2 f narray marray)
                    | _ -> raise (TyExcept "expected floats!")

let apply_string_func (f : string -> string -> string) (e : eidosValue) (e1 : eidosValue)
               = match (e, e1) with
                     | (String starray, String starray2) -> String (Array.map2 f starray starray2)
                     | _ -> raise (TyExcept "expected strings!")

let apply_eidos_func (f : eidosValue -> eidosValue -> eidosValue) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                    | (VObject v1, VObject v2) -> VObject (Array.map2 f v1 v2)
                    | _ -> raise (TyExcept "expected eidosValues!")

let rec string_of_eidos_val (evalue : eidosValue) : string = match evalue with
                        | Void   -> ""
                        | Null _ -> "null"
                        | Logical l      -> (match Array.to_list l with
                                             | [] -> ""
                                             | (x::xs) -> if x == true then "true" ^ string_of_eidos_val (Logical (Array.of_list xs))
                                                               else "false" ^ string_of_eidos_val (Logical (Array.of_list xs)))
                        | Integer l  -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_int x ^" "^(string_of_eidos_val (Integer (Array.of_list xs))))
                        | Float l    -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_float x ^ (string_of_eidos_val (Float (Array.of_list xs))))
                        | String l   -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> x ^ (string_of_eidos_val (String (Array.of_list xs))))
                        | Num l      -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_numeric x ^ (string_of_eidos_val (Num (Array.of_list xs))))
                        | VPlus l    -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_plus_op_val x ^ (string_of_eidos_val (VPlus (Array.of_list xs))))
                        | VTimes l   -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_eidos_val x ^ (string_of_eidos_val (VTimes (Array.of_list xs))))


and string_of_plus_op_val (plop : plus_op_value) : string = match plop with
                                                    | N l -> string_of_eidos_val (Null l)
                                                    | L l -> string_of_eidos_val (Logical l)
                                                    | I l -> string_of_eidos_val (Integer l)
                                                    | F l -> string_of_eidos_val (Float l)
                                                    | S l -> string_of_eidos_val (String l)
                                                    | P l -> string_of_eidos_val (VPlus l)
                                                    | T l -> string_of_eidos_val (VTimes l)

and string_of_numeric (num : numeric) : string  = match num with
                                         Int n -> string_of_int n
                                         |Flt f -> string_of_float f


let rec generateSeq n m = if n > m then
                                 n :: generateSeq (n - 1) m
                              else
                              if n == m then
                                [m] else
                                   if n < m then n :: generateSeq (n + 1) m else generateSeq m n

(*this function shouldn't test for equality ex. 3.8:5.6*)                                   
let rec generateSeq_f n m = if n > m then
                                 n :: generateSeq_f (n -. 1.0) m
                              else
                              if n = m then
                                [m] else
                                   if n < m then n :: (generateSeq_f (n +. 1.0) m) else generateSeq_f m n

(*let rec generateSeq_f n m = (print_string ((string_of_float n)^" "^(string_of_float m)));if n = m then
                                [m]
                            else
                                    if n < m then
                                            n::(generateSeq_f (n +. 1.0) m)
                                    else
                                            generateSeq_f n m
*)
let rec make_seq_of_int (n : int) (m : int) =  generateSeq n m

let rec make_seq_of_float (n : float) (m : float) = (generateSeq_f n m)

let make_seq_from_num_array n m = match (n, m) with
                     (Integer n_array, Integer m_array) -> (match (n_array, m_array) with
                                         ([|n1|], [|m1|]) -> Integer (Array.of_list (make_seq_of_int n1 m1))
                                         | _               -> raise (TyExcept "vectors must be singletons!"))
                     | (Float n_array, Float m_array) -> (match (n_array, m_array) with
                                         ([|n1|], [|m1|]) -> Float (Array.of_list (make_seq_of_float n1 m1))
                                         | _               -> raise (TyExcept "vectors must be singletons!"))
                     | _                                -> raise (TyExcept "sequence function only works with numeric types!")

let interp_for_value (env : env) (s : string) (varlist : eidosValue) (st : 'a) f
                     = match varlist with
                     | Integer int_array ->
                                      (* print_string ((string_of_eidos_val varlist) ^ "\n"); *)
                                      (* Array.fold_right (fun v (new_env, v1) -> print_string ("added for loop variable " ^ s ^ " with value " ^ (string_of_eidos_val (Integer [|v|])) ^ "\n");
                                      f (Env.add s (Integer [|v|]) new_env) st) int_array (env, Void) *)
                                      Array.fold_left (fun (new_env, v1) v -> print_string ("added for loop variable " ^ s ^ " with value " ^ (string_of_eidos_val (Integer [|v|])) ^ "\n");
                                      f (Env.add s (Integer [|v|]) new_env) st) (env, Void) int_array
                     | Float float_array -> Array.fold_right (fun v (new_env, value) -> f (Env.add s (Float [|v|]) new_env) st) float_array (env, Void)
                     | Logical l_array   -> Array.fold_right (fun v (new_env, value) -> f (Env.add s (Logical [|v|]) new_env) st) l_array (env, Void)
                     | String s_array    ->  Array.fold_right (fun v (new_env, value) -> f (Env.add s (String [|v|]) new_env) st) s_array (env, Void)
                     | _                 -> raise (TyExcept "function undefined for this array type!")
