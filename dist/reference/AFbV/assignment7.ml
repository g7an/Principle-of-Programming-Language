(* 
a7a: For your first implementation, the set actor should respond to the following messages:
        - `add(v) - add value v to the set (ignore if v is already in the set)
        - `delete(v) - delete value v from the set (ignore if v is not in the set)
        - `isempty(a) - If the set is empty, send a <- True; if not, in the set send a <- False
        - `contains(v,a) - if v is in the set, send a <- True; if v is not in the set, send a <- False
*)
        let ycomb = 
          "(Fun code -> 
              Let repl = Fun self -> Fun x -> code (self self) x 
              In repl repl)";;
        let checkContains = 
          ycomb^" (Fun rec -> Fun v -> Fun lst -> 
                    If lst = [] Then False Else
                    (
                      If ((Head lst) = v) Then True 
                      Else (rec v (Tail lst))
                    )
                  )";;
        
        (* recursive version *)
        
        let delete = 
          ycomb^" (Fun rec -> Fun v -> Fun lst ->
                    (* delete without using the head parameter *)
                    If lst = [] Then lst
                    Else
                      (
                        If ((Head lst) = v) Then (Tail lst)
                        Else (Head lst)::(rec v (Tail lst)) 
                      )
                  )
                ";;
        (* answer *)
        let a7a = 
          "(Fun me -> Fun data -> 
            "^ycomb^" (Fun this -> Fun set -> Fun msg -> 
               Match msg With
               `add(v) ->
                (
                  If ("^checkContains^" v set) Then
                    set;
                    (this set)
                  Else 
                  Let new_set = (v::set) In ((Print \"OUTPUT:\"); Print new_set); (this new_set)
                )
                | `contains(p) ->
                    (
                      (
                        Let v = (Fst p) In
                        (
                          Let a = (Snd p) In 
                          (If ("^checkContains^" v set) Then (a <- True) Else (a <- False))
                        )
                      );
                      (this set)
                    )
                |  `delete(v) -> 
                      (
                        (Let new_set = ("^delete^" v set) In (Print new_set); (this new_set))
                      )
                | `isempty(a) ->
                    (
                      (
                        If (set = []) Then (a <- True) Else (a <- False)
                      );
                      (this set)
                    )
              ) data
           )"
        ;;
        
        let a7a_tester = 
        "Let set_beh = ("^a7a^")  In
        Let test_beh = Fun me -> Fun _ -> Fun _ ->  
          Let s = Create(set_beh,[]) (* The [] here is a dummy like () in OCaml *) In
            (s <- `add(1)); (s <- `delete(1)); (s <- `add(3)); (s <- `contains(3,me));
            Fun m -> (Print \"contains(3) returned \"); (Print (m))
        In Let test = Create(test_beh,0)
        In test <- []";;
        
        (* 
        a7b. In this question, the code is designed to **add** a value 3 to the set, then check if the set **contains** 3.
            However, since the arrival order is non-deterministic, the contains may arrive before the add. 
            Therefore, False may be sent to actor 1, indicating that the set does not contain 3.
        
        G0:
        ~~~ Actors:
        "@1":...
        ~~~ Messages:
        "@1" <- 
        []
        
        G1:
        ~~~ Actors:
        "@1":...
        "@2":...
        ~~~ Messages:
        "@2" <- 
        `contains (3, (<Actor @1/>))
        "@2" <- `add 3
        "@2" <- `delete 1
        "@2" <- `add 1
        
        []
        
        G2:
        ~~~ Actors:
        "@1":...
        "@2":...
        ~~~ Messages:
        "@2" <- 
        `contains (3, (<Actor @1/>))
        "@2" <- `add 3
        "@2" <- `add 1
        
        G3:
        ~~~ Actors:
        "@1":...
        "@2":...
        ~~~ Messages:
        "@1" <- 
        False
        "@2" <- `add 3
        "@2" <- `add 1
        
        OUTPUT:
        [3]
        
        G4:
        ~~~ Actors:
        "@1":...
        "@2":...
        ~~~ Messages:
        "@1" <- 
        False
        "@2" <- `add 1
        
        contains(3) returned 
        False
        
        
        G5:
        ~~~ Actors:
        "@1":False
        "@2":...
        ~~~ Messages:
        "@2" <- 
        `add 1
        
        OUTPUT:
        [1; 3]
        
        
        
        
        *)
        
        (* a7c.
        We can see that in the states, there are 4 messages simultanuously. This could explain why actor is arbitrarily processing the messages.
        e = (test <- [])
        S = {test @1 <- []} \union {test @1, test_beh} \union {s @2 <- `add(1)} \union {s @2 <- `delete(1)} \union {s @2 <- `add(3)}
        \union {s @2 <- `contains(3, (<Actor @1/>))} 
        v = [1;3] or [3]
        
        *)
        
        let a7d : string = 
          "(Fun me -> Fun data -> 
            "^ycomb^" (Fun this -> Fun set -> Fun msg -> 
               Match msg With
               `add(p) ->
                (
                  (
                    Let v = (Fst p) In
                    (
                      Let a = (Snd p) In 
                      (
                        If ("^checkContains^" v set) Then
                            (a <- `ack([]));
                            (this set)
                        Else 
                            Let new_set = (v::set) In (this new_set); (a <- `ack([]))
                      )
                    )
                  )
                )
                | `contains(p) ->
                    (
                      (
                        Let v = (Fst p) In
                        (
                          Let a = (Snd p) In 
                          (If ("^checkContains^" v set) Then (Print v); (a <- True) Else (Print v); (a <- False))
                        )
                      );
                      (this set)
                    )
                |  `delete(p) -> 
                      (
                        (
                          Let v = (Fst p) In
                          (
                            Let a = (Snd p) In 
                            (
                              Let new_set = ("^delete^" v set) In (Print new_set); a <- `ack([]); (this new_set)
                            )
                          )
                        )
                      )
                | `isempty(a) ->
                    (
                      (
                        If (set = []) Then (a <- True) Else (a <- False)
                      );
                      (this set)
                    )
                | `ack(v) -> 0
              ) data
           )"
        ;;
        let a7d_tester : string =
          "Let set_beh = ("^a7d^") In
           Let test_beh = Fun me -> Fun _ -> Fun _ ->  
             Let s = Create(set_beh,[]) (* The [] here is a dummy like () in OCaml *) In
               (s <- `add(1, me)); 
               (Fun m -> 
                  Match m With
                  `ack(v) -> (s <- `delete(1, me)); Print \"Acked add(1), send delete1\"
                );
                (Fun m -> 
                  Match m With
                  `ack(v) -> (s <- `add(3, me)); Print \"Acked delete(1), send add3\"
                );
                (Fun m -> 
                  Match m With
                  `ack(v) -> (s <- `contains(3, me)); Print \"Acked add(3), send contains\"
                );
                (Fun m -> (Print m))
           In Let test = Create(test_beh,0)
           In test <- []"
          ;;