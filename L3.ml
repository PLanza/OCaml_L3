open Syntax;;
open Interpreter;;
open PrettyPrint;;

(* 
  let m: (int -> int) -> (int * int) -> int -> int =
    fn f: (int -> int) => fn p: (int * int) => fn i: int =>
      if 0 >= i then
        (f (#1 p), #2 p)
      else
        (#1 p, (#2 p))
  in 
    m (fn x:int => 10 + x) (2, 3) 1
*)
let e1 = Let(Ty_func(Ty_func(Ty_func(Ty_func(Ty_int, Ty_int), Ty_product(Ty_int, Ty_int)), Ty_int), Ty_int),
            Fn(Ty_func(Ty_int, Ty_int), Fn(Ty_product(Ty_int, Ty_int), Fn(Ty_int, 
              If(Op (Integer 0, GTEQ, Var 0),
                Product(App(Var 2, (Fst(Var 1))), Snd(Var 1)),
                Product(Fst(Var 1), App(Var 2, (Snd(Var 1))))
              )
            ))),
            App(App(App(Var 0, Fn(Ty_int, Op(Integer 10, Plus, Var 0))), Product(Integer 2, Integer 3)), Integer (1))
         )

let e2 = Let (Ty_record([("a", Ty_int); ("b", Ty_int); ("c", Ty_int)]),
              Ref(Record([("a", Op(Integer 3, Plus, Integer (-2))); ("b", Integer 2); ("c", Op(Integer 1, Plus, Integer 2))])),
              Op(Project("a", Deref (Var 0)), Plus, Op(Project("b", Deref (Var 0)), Plus, Project("c", Deref (Var 0))))
             )

let e3 = Let(Ty_ref(Ty_sum(Ty_int, Ty_unit)), Ref(Inl(Integer 0, Ty_sum(Ty_int, Ty_unit))),
         Let(Ty_ref(Ty_int), Ref(Integer 0),
         Letrecfn(Ty_func(Ty_func(Ty_func(Ty_int, Ty_int), Ty_int),Ty_int),
                  Ty_func(Ty_int, Ty_int), Fn(Ty_int,
                                              Case(Deref(Var 4), 
                                                   Ty_int, Seq(If(Op(Var 0, GTEQ, Var 1),
                                                                  Assign(Var 5, Inr(Skip, Ty_sum(Ty_int, Ty_unit))),
                                                                  Seq(Assign(Var 4, App(Var 2, Deref(Var 4))),
                                                                      Assign(Var 5, Inl(Op(Var 0, Plus, Integer 1), Ty_sum(Ty_int, Ty_unit)))
                                                                     )
                                                                 ), 
                                                               App(App(Var 3, Var 2), Var 1)
                                                              ),
                                                   Ty_unit, Deref(Var 4)
                                                  )
                                               ),
                  App(App(Var 0, Fn(Ty_int, Op(Integer 10, Plus, Var 0))), Integer 5)
                 )
         ))

let _ = prettyreduce e3