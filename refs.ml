(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2017
 *)


(* The type of mutable lists. *)
type 'a mlist = Nil
              | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)

let rec list_iter (vis : 'a mlist ref list)
              (r : 'a mlist ref)
              (fNil : 'a mlist ref list -> 'b)
              (fCons : 'a -> 'a mlist ref -> 'a mlist ref list -> 'b)
            : 'b =
  match !r with
  | Nil -> fNil vis
  | Cons (x, y) ->
      if List.memq r vis
      then fCons x r vis
      else list_iter (r::vis) y fNil fCons

let has_cycle (lst : 'a mlist) : bool =
  list_iter [] (ref lst) (fun _ -> false) (fun _ _ _ -> true)
(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)
let flatten2 (lst : 'a mlist) : unit =
  list_iter [] (ref lst) (fun _ -> ()) (fun x n _ -> n := Cons (x, ref Nil) )

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  list_iter [] (ref lst) (fun ns -> List.length ns)
  (fun _ _ ns -> (List.length ns) - 1 )





(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
