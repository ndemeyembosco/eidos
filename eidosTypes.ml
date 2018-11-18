(* all these are constructors for arrayor values in ocaml. *)
exception Nil



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
              | CustomObj of (customObj array)
              | List of (eidosValue list)

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
               | O of (customObj array)

(* numeric is the union of string and float types *)
and numeric = Str of string | Flt of float

(* a user defined types name. This representation is almost
certainly wrong. But since it is eidos environment dependent
I believe if should be a pair of the name of the object
and its class/data type.  *)

and customObj = Object of (string * eidosValue)

(* our value environment is a mapping from variable names to their values.  *)
module Env = Map.Make(String)

type env   = eidosValue Env.t
let empty_env : env = Env.empty
