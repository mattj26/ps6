(**********************************************************************
			  CS51 Problem Set 6
			     Spring 2016
		       Refs, Streams, and Music

			 Part 1: Refs Testing
 **********************************************************************)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons(2, ref Nil) ;;
let list1b = Cons(2, ref list1a) ;;
let list1 = Cons(1, ref list1b) ;;

let reflist = ref (Cons(2, ref Nil)) ;;
let list2 = Cons(1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

(* Some example tests. You'll want more. *)
let _ = assert(not(has_cycle list1a)) ;;
let _ = assert(has_cycle(!reflist)) ;;
