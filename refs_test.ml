(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                             Refs Testing
                             Spring 2017
 *)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let list3ref = ref Nil
let list3 = Cons (8, ref ( Cons( 2, list3ref)))
let list4 = Cons (4, list3ref)
let list5 = Cons (10, ref list1)

let _ = reflist := list2 ;;
let _ =  list3ref := list3;;
(* Some example tests. You'll want more. *)

let test_cycle () =
  assert (not (has_cycle list1a));
  assert (not (has_cycle list1));
  assert (has_cycle list2);
  assert (has_cycle list3);
  assert (has_cycle list4);
  assert (not (has_cycle list5));
  print_endline "Passed test cycle"

let test_length () =
  assert (mlength list1a = 1);
  assert (mlength list1b = 2);
  assert (mlength list1 = 3);
  assert (mlength list2 = 2);
  assert (mlength list3 = 2);
  assert (mlength list4 = 3);
  print_endline "Passed test length"

let test_flatten () =
  let lstRef = ref (Cons (5, ref Nil)) in
  let lst1 = Cons(1, ref (Cons (1, lstRef))) in
  lstRef := lst1;
  assert (has_cycle lst1);
  flatten lst1;
  assert (not(has_cycle lst1));
  assert (lst1 = Cons(1, ref (Cons (1, ref Nil))));
  print_endline "Passed test flatten"

let _ =
  test_cycle ();
  test_length();
  test_flatten ()
