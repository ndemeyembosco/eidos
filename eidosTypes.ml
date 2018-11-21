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


let apply_int_func (f : int -> int -> int) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                     | (Integer narray, Integer marray) -> Integer (Array.map2 f narray marray)
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

let apply_array_func (f : 'a array -> 'a array -> 'a array) (e : eidosValue) (e1 : eidosValue)
                = match (e, e1) with
                    | (VObject v1, VObject v2) -> VObject (f v1 v2)
                    | _ -> raise (TyExcept "expected VObject eidosValues!")

let rec string_of_eidos_val (evalue : eidosValue) : string = match evalue with
                        | Void   -> "void"
                        | Null _ -> "null"
                        | Logical l      -> (match Array.to_list l with
                                             | [] -> ""
                                             | (x::xs) -> if x then "true" ^ string_of_eidos_val (Logical (Array.of_list xs))
                                                               else "false" ^ string_of_eidos_val (Logical (Array.of_list xs)))
                        | Integer l  -> (match Array.to_list l with
                                            | [] -> ""
                                            | (x::xs) -> string_of_int x ^ (string_of_eidos_val (Integer (Array.of_list xs))))
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
                        | _          -> "unable to convert to string!"


and string_of_plus_op_val (plop : plus_op_value) : string = match plop with
                                                    | N l -> string_of_eidos_val (Null l)
                                                    | L l -> string_of_eidos_val (Logical l)
                                                    | I l -> string_of_eidos_val (Integer l)
                                                    | F l -> string_of_eidos_val (Float l)
                                                    | S l -> string_of_eidos_val (String l)
                                                    | P l -> string_of_eidos_val (VPlus l)
                                                    | T l -> string_of_eidos_val (VTimes l)
                                                    | Numeric l -> string_of_eidos_val (Num l)







and string_of_numeric (num : numeric) : string  = match num with
                                         Int n -> string_of_int n
                                         |Flt f -> string_of_float f

let rec make_seq_of_int (n : int) (m : int) =  if n >= m then [n] else n :: make_seq_of_int (n + 1) m

let rec make_seq_of_float (n : float) (m : float) = if n >= m then [n] else n :: make_seq_of_float (n +. 1.0) m

let make_seq_from_num_array n m = match (n, m) with
                      (Integer n_array, Integer m_array) -> (match (n_array, m_array) with
                                          ([|n1|], [|m1|]) -> Integer (Array.of_list (make_seq_of_int n1 m1))
                                          | _               -> raise (TyExcept "vectors must be singletons!"))
                      | (Float n_array, Float m_array) -> (match (n_array, m_array) with
                                          ([|n1|], [|m1|]) -> Float (Array.of_list (make_seq_of_float n1 m1))
                                          | _               -> raise (TyExcept "vectors must be singletons!"))
                      | _                                -> raise (TyExcept "sequence function only works with numeric types!")



let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
