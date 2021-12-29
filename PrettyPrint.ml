open Syntax;;
open Interpreter;;

let pp_op op = 
  match op with
  | Plus -> "+"
  | GTEQ -> ">="

let rec pp_type t = 
  match t with 
  | Ty_int -> "int"
  | Ty_bool -> "bool"
  | Ty_unit -> "unit"
  | Ty_func (t1, t2) -> "(" ^ (pp_type t1) ^ " -> " ^ (pp_type t2) ^ ")"
  | Ty_product (t1, t2) -> "(" ^ (pp_type t1) ^  " * " ^ (pp_type t2) ^ ")"
  | Ty_sum (t1, t2) -> "(" ^ (pp_type t1) ^  " + " ^ (pp_type t2) ^ ")"
  | Ty_record xs ->
    let rec pp_record xs =
      (match xs with
      | [] -> ""
      | (lab, t)::xs -> lab ^ "=" ^ (pp_type t) ^ ", " ^ (pp_record xs)
      )
    in "{" ^ (pp_record xs) ^ "}"
  | Ty_ref (t) -> (pp_type t) ^ "ref"

let rec pp_expr e = 
  match e with 
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Op (e1, op, e2) -> 
    "(" ^ (pp_expr e1) ^ (pp_op op) ^ (pp_expr e2) ^ ")"
  | If (e1, e2, e3) -> 
    "(if " ^ (pp_expr e1) ^ " then " ^ (pp_expr e2) 
      ^ " else " ^ (pp_expr e3) ^ ")"
  | Loc l -> string_of_int l
  | Assign (e1, e2) -> "(" ^ (pp_expr e1) ^ ":=" ^ (pp_expr e2) ^ ")"
  | Deref e -> "!" ^ (pp_expr e)
  | Ref e -> "ref " ^(pp_expr e) 
  | Skip -> "skip"
  | Seq (e1, e2) -> "(" ^ (pp_expr e1) ^ "; " ^ (pp_expr e2) ^ ")"
  | While (e1, e2) -> "while " ^ (pp_expr e1) ^ " do " ^ (pp_expr e2)
  | Var n -> "var_" ^ string_of_int n
  | Fn (t, e1) -> "(fn . :" ^ (pp_type t) ^ "=>" ^ (pp_expr e1) ^ ")"
  | App (e1, e2) -> "(" ^ (pp_expr e1) ^ " " ^ (pp_expr e2) ^ ")"
  | Let (t, e1, e2) -> 
    "let val . : " ^ (pp_type t) ^ " = " ^ (pp_expr e1) ^ " in " 
    ^ (pp_expr e2) ^ " end "
  | Letrecfn (tx, ty, e1, e2) -> 
    "let val rec . :" ^ (pp_type tx) ^ " = (fn . :" ^ (pp_type ty) ^ " => "
    ^ (pp_expr e1) ^ ") in " ^ (pp_expr e2) ^ " end"
  | Product (e1, e2) -> "(" ^ (pp_expr e1) ^ "," ^ (pp_expr e2) ^ ")"
  | Fst e -> "#1 " ^ (pp_expr e)
  | Snd e -> "#2 " ^ (pp_expr e)
  | Inl (e, t) -> "inl " ^ (pp_expr e) ^ ":" ^ (pp_type t)
  | Inr (e, t) -> "inr " ^ (pp_expr e) ^ ":" ^ (pp_type t)
  | Case (e, tl, e1, tr, e2) ->
    "case " ^ (pp_expr e) ^ " of inl (x:" ^ (pp_type tl) ^ ") =>" ^ (pp_expr e1)
    ^ " | inr (y:" ^ (pp_type tr) ^ ") =>" ^ (pp_expr e2)
  | Record xs -> 
    let rec pp_record xs =
      (match xs with
      | [] -> ""
      | (lab, e)::xs -> lab ^ "=" ^ (pp_expr e) ^ ", " ^ (pp_record xs)
      )
    in "{" ^ (pp_record xs) ^ "}"
  | Project (lab, e) -> "#" ^ lab ^ " " ^ (pp_expr e)


let rec pp_fold f xs = 
  match xs with 
  | [] -> ""
  | x::[] -> f x
  | x::xs -> (f x) ^ ", " ^ (pp_fold f xs)

let pp_store_plain s =
  pp_fold (fun (l, v) -> "l" ^ (string_of_int l) ^ "=" ^ (pp_expr v)) s

let rec pp_store pairs = 
  let pairs' = List.sort 
    (fun (l,n) (l',n') -> compare l l')
    pairs
in "{" ^ pp_store_plain pairs' ^ "}"

let pp_config (e, s) =
  "< " ^ (pp_expr e) ^ ", " ^ (pp_store s) ^ " >"

let rec prettyreduce' (e, s) =
  match reduce (e, s) with 
  | Some (e', s') -> 
    (Printf.printf "%s" ("\n --> " ^ pp_config (e', s'));
    prettyreduce' (e', s') )
  | None -> 
    (Printf.printf "\n -/-> ";
    if is_value e then
      Printf.printf "(a value)\n"
    else 
      Printf.printf "(stuck - not a value)" )

let rec prettyreduce (e, s) = 
  (Printf.printf "%s" ("     " ^ pp_config (e, s));
    prettyreduce' (e, s) )