type label = string

type loc = int

type oper = Plus | GTEQ

type type_L3 = 
  | Ty_int
  | Ty_unit
  | Ty_bool
  | Ty_func of type_L3 * type_L3
  | Ty_product of type_L3 * type_L3
  | Ty_sum of type_L3 * type_L3
  | Ty_record of (label * type_L3) list
  | Ty_ref of type_L3 

type type_loc = Ty_ref

type expr = 
  | Integer of int
  | Boolean of bool
  | Op of expr * oper * expr
  | If of expr * expr * expr
  | Loc of loc
  | Assign of expr * expr
  | Deref of expr
  | Ref of expr
  | Skip
  | Seq of expr * expr
  | While of expr * expr
  | Var of int
  | Fn of type_L3 * expr
  | App of expr * expr
  | Let of type_L3 * expr * expr
  | Letrecfn of type_L3 * type_L3 * expr * expr
  | Product of expr * expr
  | Fst of expr
  | Snd of expr
  | Inl of expr * type_L3 
  | Inr of expr * type_L3 
  | Case of expr * type_L3 * expr * type_L3 * expr
  | Record of (label * expr) list
  | Project of label * expr

let rec is_value v = 
  match v with 
  | Integer n -> true
  | Boolean b -> true
  | Skip -> true
  | Fn (t, e) -> true
  | Product (v1, v2) -> (is_value v1) && (is_value v2)
  | Inl (v, t) -> is_value v
  | Inr (v, t) -> is_value v
  | Record xs -> 
    (match xs with
    | [] -> true
    | (l, v)::xs -> (is_value v) && (is_value (Record xs))
    )
  | Loc l -> true
  | _ -> false

type store = (loc * expr) list