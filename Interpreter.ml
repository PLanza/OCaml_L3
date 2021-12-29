open Syntax;;

let rec lookup s l =
  match s with
  | [] -> None
  | (l', n')::s' -> 
    if l = l' then 
      Some n' 
    else 
      lookup s' l

let rec update' front s (l, v) = 
  match s with 
  | [] -> None
  | (l', n')::s' ->
    if l = l' then 
      Some(front @ ((l, v)::s'))
    else 
      update' ((l', n')::front) s' (l, v)

let update s (l, v) = update' [] s (l, v)

let rec new_ref' front s v i =
  match s with 
  | [] -> ((i, v)::front, i)
  | (l', v')::s' -> new_ref' ((l', v')::front) s' v (i+1)

let new_ref s v = new_ref' [] s v 0

let rec record_lookup record lab =
  (match record with 
  | [] -> None
  | (lab', v)::record -> 
    if lab' = lab then 
      Some (v)
    else 
      record_lookup record lab
  )

let rec subst e n expr = 
  match expr with 
  | Integer n' -> Integer n'
  | Boolean b -> Boolean b
  | Op (e1, op, e2) -> Op (subst e n e1, op, subst e n e2)
  | If (e1, e2, e3) -> If (subst e n e1, subst e n e2, subst e n e3)
  | Loc l -> Loc l
  | Assign (e1, e2) -> Assign (subst e n e1, subst e n e2)
  | Deref (e1) -> Deref (subst e n e1)
  | Ref (e1) -> Ref (subst e n e1)
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (subst e n e1, subst e n e2)
  | While (e1, e2) -> While (subst e n e1, subst e n e2)
  | Var x -> 
    (if x=n then 
      e
    else
      Var x )
  | Fn (t, e1) -> Fn (t, subst e (n+1) e1)
  | App (e1, e2) -> App (subst e n e1, subst e n e2)
  | Let (t, e1, e2) -> Let (t, subst e n e1, subst e (n+1) e2)
  | Letrecfn (tx, ty, e1, e2) -> 
    Letrecfn (tx, ty, subst e (n+2) e1, subst e (n+1) e2)
  | Product (e1, e2) -> Product (subst e n e1, subst e n e2)
  | Fst (e1) -> Fst (subst e n e1)
  | Snd (e1) -> Snd (subst e n e1)
  | Inl (e1, t) -> Inl (subst e n e1, t)
  | Inr (e1, t) -> Inr (subst e n e1, t)
  | Case (e1, tl, e2, tr, e3) -> 
    Case (subst e n e1, tl, subst e (n+1) e2, tr, subst e (n+1) e3)
  | Record xs -> 
    (match List.split xs with 
    | (labs, es) -> Record (List.combine labs (List.map (subst e n) es))
    )
  | Project (lab, e1) -> Project (lab, subst e n e1)

(* 
 * The shift and swap auxiliary functions are required for manipulating 
 * De Bruijn indices in the Letrecfn case of reduce below.
 *)
 let rec shift n expr =
  match expr with
  | Integer n' -> Integer n'
  | Boolean b -> Boolean b
  | Op (e1, op, e2) -> Op (shift n e1, op, shift n e2)
  | If (e1, e2, e3) -> If (shift n e1, shift n e2, shift n e3)
  | Loc l -> Loc l
  | Assign (e1, e2) -> Assign (shift n e1, shift n e2)
  | Deref (e1) -> Deref (shift n e1)
  | Ref (e1) -> Ref (shift n e1)
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (shift n e1, shift n e2)
  | While (e1, e2) -> While (shift n e1, shift n e2)
  | Var x -> 
    (if x >= n then
      Var (x+1)
    else 
      Var x )
  | Fn (t, e1) -> Fn (t, shift (n+1) e1)
  | App (e1, e2) -> App (shift n e1, shift n e2)
  | Let (t, e1, e2) -> Let (t, shift n e1, shift (n+1) e2)
  | Letrecfn (tx, ty, e1, e2) -> 
    Letrecfn (tx, ty, shift (n+2) e1, shift (n+1) e2)
  | Product (e1, e2) -> Product (shift n e1, shift n e2)
  | Fst (e1) -> Fst (shift n e1)
  | Snd (e1) -> Snd (shift n e1)
  | Inl (e1, t) -> Inl (shift n e1, t)
  | Inr (e1, t) -> Inr (shift n e1, t)
  | Case (e1, tl, e2, tr, e3) -> 
    Case (shift n e1, tl, shift (n+1) e2, tr, shift (n+1) e3)
  | Record xs -> 
    (match List.split xs with 
    | (labs, es) -> Record (List.combine labs (List.map (shift n) es))
    )
  | Project (lab, e1) -> Project (lab, shift n e1)

let rec swap n expr =
  match expr with
  | Integer n' -> Integer n'
  | Boolean b -> Boolean b
  | Op (e1, op, e2) -> Op (swap n e1, op, swap n e2)
  | If (e1, e2, e3) -> If (swap n e1, swap n e2, swap n e3)
  | Loc l -> Loc l
  | Assign (e1, e2) -> Assign (swap n e1, swap n e2)
  | Deref (e1) -> Deref (swap n e1)
  | Ref (e1) -> Ref (swap n e1)
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (swap n e1, swap n e2)
  | While (e1, e2) -> While (swap n e1, swap n e2)
  | Var x -> 
    (if x = n then
      Var (n+1)
    else 
      if x = n+1 then
        Var n
      else 
        Var x )
  | Fn (t, e1) -> Fn (t, swap (n+1) e1)
  | App (e1, e2) -> App (swap n e1, swap n e2)
  | Let (t, e1, e2) -> Let (t, swap n e1, swap (n+1) e2)
  | Letrecfn (tx, ty, e1, e2) -> 
    Letrecfn (tx, ty, swap (n+2) e1, swap (n+1) e2)
  | Product (e1, e2) -> Product (swap n e1, swap n e2)
  | Fst (e1) -> Fst (swap n e1)
  | Snd (e1) -> Snd (swap n e1)
  | Inl (e1, t) -> Inl (swap n e1, t)
  | Inr (e1, t) -> Inr (swap n e1, t)
  | Case (e1, tl, e2, tr, e3) -> 
    Case (swap n e1, tl, swap (n+1) e2, tr, swap (n+1) e3)
  | Record xs -> 
    (match List.split xs with 
    | (labs, es) -> Record (List.combine labs (List.map (swap n) es))
    )
  | Project (lab, e1) -> Project (lab, swap n e1)

exception Reduce of string

 let rec reduce (e, s) = 
  match e with
  | Integer n -> None
  | Boolean b -> None
  | Op (e1, op, e2) -> 
    (match (e1, op, e2) with 
    | (Integer n1, Plus, Integer n2) -> Some (Integer (n1 + n2), s)      (* op+ *)
    | (Integer n1, GTEQ, Integer n2) -> Some (Boolean (n1 >= n2), s)     (* op>= *)
    | (e1, opr, e2) -> 
      (if is_value e1 then 
        (match reduce (e2, s) with
        | Some (e2', s') -> Some (Op (e1, op, e2'), s')                  (* op2 *)
        | None -> None)
      else
        (match reduce (e1, s) with
        | Some (e1', s') -> Some (Op (e1', op, e2), s')                  (* op1 *)
        | None -> None)
      )
    )
  | If (e1, e2, e3) -> 
    (match e1 with
    | Boolean true -> Some (e2, s)                                       (* if1 *)
    | Boolean false -> Some (e3, s)                                      (* if2 *)
    | _ -> 
      (match reduce (e1, s) with
      | Some (e1', s') -> Some (If (e1', e2, e3), s')                    (* if3 *)
      | None -> None)
    )
  | Loc l -> None
  | Deref l -> 
    (match l with 
    | Loc l -> 
      (match  lookup s l with
      | Some v -> Some (v, s)                                            (* deref1 *)
      | None -> None)
    | l -> 
      (match reduce (l, s) with 
      | Some (e', s') -> Some (Deref e', s')                             (* deref2 *)
      | None -> None)
    )
  | Assign (l, e) -> 
    (match l with
    | Loc l -> 
      if (is_value e) then
        (match update s (l, e) with
        | Some s' -> Some (Skip, s')                                     (* assign1 *)
        | None -> None )
      else 
        (match reduce (e, s) with
        | Some (e', s') -> Some (Assign (Loc l, e'), s')                 (* assign3*)
        | None -> None )
    | l ->  
      (match reduce (l, s) with 
      | Some (l', s') -> Some (Assign (l', e), s')                       (* assign2 *)
      | None -> None )
    )
  | Ref v -> 
    if is_value v then
      (match new_ref s v with
      | (s', l) -> Some (Loc l, s'))                                     (* ref1 *)
    else 
      (match reduce (v, s) with
      | Some (e', s') -> Some (Ref e', s')                               (* ref 2 *)
      | None -> None)
  | Skip -> None
  | Seq (e1, e2) -> 
    (match e1 with
    | Skip -> Some (e2, s)                                               (* seq1 *)
    | _ -> 
      (match reduce (e1, s) with 
      | Some (e1', s') -> Some (Seq (e1', e2), s')                       (* seq2 *)
      | None -> None)
    )
  | While (e1, e2) -> Some (If (e1, Seq(e2, While(e1, e2)), Skip), s)    (* while *)
  | Var n -> raise (Reduce "bogus unbound Var")
  | Fn (t, e) -> None
  | App (e1, e2) -> 
    (match e1 with
    | Fn (t, e) -> 
      (if (is_value e2) then
        Some (subst e2 0 e, s)                                           (* fn *)
      else 
        (match reduce (e2, s) with
        | Some (e2', s') -> Some (App (e1, e2'), s')                     (* app2 *)
        | None -> None )
      )
    | _ ->
      (match reduce (e1, s) with 
      | Some (e1', s') -> Some (App (e1', e2), s')                       (* app1 *)
      | None -> None )
    )
  | Let (t, e1, e2) -> 
    (if is_value e1 then 
      Some (subst e1 0 e2, s)                                            (* let2 *)
    else 
      (match reduce (e1, s) with
      | Some (e1', s') -> Some (Let (t,e1', e2), s')                     (* let1 *)
      | None -> None )
    )
  | Letrecfn (tx, ty, e1, e2) ->
    let e = Fn(ty, Letrecfn (tx, ty, shift 2 e1, swap 0 e1)) in
      Some (subst e 0 e2, s)                                             (* letrecfn *)
  | Product (e1, e2) -> 
    (match reduce (e1, s) with
    | Some (e1', s') -> Some (Product (e1', e2), s')                     (* pair1 *)
    | _ -> 
      (match reduce (e2, s) with 
      | Some (e2', s') -> Some (Product (e1, e2'), s')                   (* pair2 *)
      | None -> None )
    )
  | Fst e -> 
    if is_value e then
      (match e with
      | Product (v1, v2) -> Some (v1, s)                                 (* proj1 *)
      | _ -> None )
    else 
      (match reduce (e, s) with
      | Some (e', s') -> Some (Fst e', s')                               (* proj3 *)
      | None -> None ) 
  | Snd e -> 
    if is_value e then
      (match e with
      | Product (v1, v2) -> Some (v2, s)                                 (* proj2 *)
      | _ -> None )
    else 
      (match reduce (e, s) with
      | Some (e', s') -> Some (Snd e', s')                               (* proj4 *)
      | None -> None ) 
  | Inl (e, t) -> 
    (match reduce (e, s) with
      | Some (e', s') -> Some (Inl (e', t), s')                          (* inl *)
      | None -> None ) 
  | Inr (e, t) -> 
    (match reduce (e, s) with
      | Some (e', s') -> Some (Inr (e', t), s')                          (* inr *)
      | None -> None ) 
  | Case (e, tl, e1, tr, e2) -> 
    if is_value e then
      (match e with 
      | Inl (v, t) -> Some (subst v 0 e1, s)                             (* case2 *)
      | Inr (v, t) -> Some (subst v 0 e2, s)                             (* case3 *)
      | _ -> None )
    else 
      (match reduce (e, s) with
      | Some (e', s') -> Some (Case (e', tl, e1, tr, e2), s')            (* case1 *)
      | None -> None ) 
  | Record xs -> 
    let rec reduce_record front back = 
      (match back with
      | [] -> None
      | (lab, e)::back -> 
        if is_value e then 
          reduce_record ((lab, e)::front) back 
        else 
          (match reduce (e, s) with 
          | Some (e', s') -> 
            Some (Record (List.rev_append front ((lab, e')::back)), s')  (* record1 *)
          | None -> None )
      )
    in reduce_record [] xs
  | Project (lab, e) -> 
    if is_value e then
      (match e with
      | Record xs -> 
        (match record_lookup xs lab with 
        | Some (v) -> Some (v, s)                                        (* record2*)
        | None -> None )
      | _ -> None )
    else 
      (match reduce (e, s) with 
      | Some (e', s') -> Some (Project (lab, e'), s')                    (* record3 *)
      | None -> None )

let rec evaluate' (e, s) =
  match reduce (e, s) with 
  | None -> (e, s)
  | Some (e', s') -> evaluate' (e', s')

let evaluate e = evaluate' (e, [])