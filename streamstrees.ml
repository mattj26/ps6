(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                      Section 2: Lazy Evaluation
                             Spring 2017
 *)
(*======================================================================
Section 2.1: Series acceleration with infinite streams

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approxiating pi through partial sums of the terms
in a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi.

Recall from lecture the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi.  This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream.
......................................................................*)

open NativeLazyStreams ;;

(*......................................................................
Problem 5: Implementing average on streams

Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
......................................................................*)

let rec average (s : float stream) : float stream =
  let hd, tl = head s, tail s in
  lazy (Cons ((hd +. head tl) /.  2.,  average tl))

(*......................................................................
Problem 6: Implementing Aitken's method

Now instead of using the stream of approximations in pi_sums, you can
instead use the stream of averaged pi_sums, which converges much more
quickly. Test that it requires far fewer steps to get within, say,
0.001 of pi.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
let avg_pi_sums = average pi_sums


let rec aitken (s: float stream) : float stream =
  let n2 = head s in
  let n1 = head (tail s) in
  let n = head (tail (tail s)) in
  lazy (Cons (n -. (n -. n1) ** 2. /. (n -. 2. *. n1 +. n2), aitken (tail s)))

(*......................................................................
Problem 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method
    ---------------------------------------------------------
    0.1      |    19     |          2        |      0
    ---------------------------------------------------------
    0.01     |    199    |         9         |      2
    ---------------------------------------------------------
    0.001    |   1999    |         30        |      6
    ---------------------------------------------------------
    0.0001   |   19999   |        99         |      15
    ---------------------------------------------------------
......................................................................*)

let print_int_float (tup : int * float) : unit =
  match tup with
  | i, f -> Printf.printf "Steps taken: %i  ->Value: %f\n" i f

let testing () =
  let eps = [0.1; 0.01; 0.001; 0.0001] in
  let ait = aitken pi_sums in
  List.iter (fun e -> Printf.printf "Epsilon is %f" e;
                      print_int_float (within e ait)) eps

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
tree holds a value of some type 'a, and a (conventional, finite) list
of one or more (lazy) child trees. Complete the implementation by
writing print_depth, tmap, tmap2, and bfenumerate.  We recommend
implementing them in that order.
......................................................................*)

type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 8: Implementing infinite trees
*)

(*......................................................................
node t -- Returns the element of type 'a stored at the root node of
tree t of type 'a tree.
......................................................................*)
let node (t : 'a tree) : 'a =
  let Node (v, _) = Lazy.force t in
  v;;

(*......................................................................
children t -- Returns the list of children of the root node of tree t.
......................................................................*)
let children (t : 'a tree) : 'a tree list =
  let Node (_, chil) = Lazy.force t in
  if List.length chil < 1
  then raise Finite_tree
  else chil

(*......................................................................
print_depth n indent t -- Prints a representation of the first n
levels of the tree t indented indent spaces. You can see some examples
of the intended output of print_depth below.
......................................................................*)
let print_depth (num : int) (indent : int) (t : int tree) : unit =
  let rec inner_depth (count : int) (t2 : int tree) : unit =
      let n, c = node t2, children t2 in
      let str = String.make (count * indent + count) ' ' in
      print_endline (str ^ string_of_int n);
      if count + 1 < num
      then
        List.iter (fun x -> inner_depth (count + 1) x) c
      else
        () in
  inner_depth 0 t

(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function f over
each node in t.
......................................................................*)
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  lazy (Node (f (node t), List.map (tmap f) (children t)));;


(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function f
to corresponding nodes in t1 and t2, which must have the same
"shape". If they don't an Invalid_argument exception is raised.
......................................................................*)
let rec tmap2 (f : 'a -> 'b -> 'c)
              (t1 : 'a tree) (t2 : 'b tree)
            : 'c tree =
  lazy ( Node (f (node t1) (node t2),
    List.map2 (tmap2 f) (children t1) (children t2) ));;

(*......................................................................
bfenumerate tslist -- Returns a LazyStreams.stream of the nodes in the
list of trees tslist enumerated in breadth first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of bfenumerate being applied below.
......................................................................
 *)
let rec bfenumerate (tslist : 'a tree list) : 'a stream =
  match tslist with
  | [] -> raise Finite_tree
  | _ ->
      let nodes = List.fold_right (fun t l -> (node t)::l) tslist [] in
      let children = List.fold_right (fun t l -> (children t) :: l) tslist []
      |> List.concat in
      let rec inner_enu (nds : 'a list) : 'a stream =
        match nds with
        | [] -> bfenumerate children
        | hd::tl -> lazy (Cons (hd, inner_enu tl)) in
      inner_enu nodes;;

(* Now use your implementation to generate some interesting infinite
trees.  Hint: Drawing a tree considering how the values change along
each branch will yield helpful intuition for the next problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
let rec onest : int tree =
  lazy (Node (1, [onest; onest]))

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument n. For example:

# print_depth 2 0 (levels 0) ;;
0...
 1...
  2...
  2...
 1...
  2...
  2...
- : unit = ()
......................................................................*)
let rec levels (n : int) : int tree =
  lazy (Node (n, [levels (n + 1); levels (n + 1)]))


(*......................................................................
Define an infinite binary tree tree_nats where the value of each node in
the tree is consecutively numbered in breadth-first order starting
with 0. For example:

# print_depth 2 0 tree_nats ;;
0...
 1...               1 -> 3,5
  3...             3 -> 7, 9
    7...
    8...
  4...          4 -> 8, 10
    9...        2 -> 4, 6
    10...
 2...
  5...
    11...
    12...
  6...
    13...
    14...
- : unit = ()
# first 10 (bfenumerate [tree_nats]) ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
let pow (b : int) (e : int) : int =
  float_of_int b ** float_of_int e |> int_of_float

let rec tree_nats : int tree =
  let lev = levels 0 in
  lazy (Node (0, (tmap2 (fun x y -> (pow 2 x) + y) lev tree_nats::
    [tmap2 (fun x y -> (pow 2 x) * 2 + y) lev tree_nats])))
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 220;;
