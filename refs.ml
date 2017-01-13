(* 
        CS 51 Problem Set 6
        Refs, Streams, and Music
        Part 1: Refs
        Spring 2017
*)

(********
CHANGE BY TIM: Changed the commenting structure to match that of the labs. Specifically, multiline comments being surrounded only by outer and inner comment markers (no * each line). 
********)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(* 
......................................................................
Write a function has_cycle that returns whether a mutable list has
a cycle. You may want a recursive helper function. Don't worry
about space usage.
......................................................................
*)
let has_cycle (lst : 'a mlist) : bool =
  failwith "has_cycle not implemented"

(* 
......................................................................
Write a function flatten that flattens a list (removes its cycles
if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space
......................................................................
*)
let flatten (lst : 'a mlist) : unit =
  failwith "flatten not implemented"

(* 
......................................................................
Write mlength, which nondestructively finds the number of nodes in
a mutable list that may have cycles.
......................................................................
*)
let mlength (lst : 'a mlist) : int =
  failwith "mlength not implemented"
(********
CHANGE BY TIM: 
Addressing comment left by Stuart: For future reference, this approach makes a flattened copy and then find the length of that. It would be better to avoid the copy and walk the list stopping upon reentries. 

I reimplemented mlength to be more efficient.
********)

(* 
Please give us an honest estimate of how long this part took you to
complete. We care about your responses and will use them to help
guide us in creating future assignments. 
*)
let minutes_spent : int = -1