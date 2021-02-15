(* Problem 1:

Write an OCaml function "zip3" that takes three lists and returns a
list of their elements combined elementwise as 3-tuples.  The result should
be the same length as the shortest given list.

E.g.,

zip3 [] [] [] = []

zip3 [1;2;3] [4;5;6] [7;8;9] = [(1, 4, 7); (2, 5, 8); (3, 6, 9)]

zip3 [1;2] ['a';'b';'c'] [0;0] = [(1, 'a', 0); (2, 'b', 0)]

*)

let rec zip3 (l1 : 'a list) (l2 : 'b list) (l3 : 'c list) : ('a * 'b * 'c) list =
    if List.length l1 == 0 || List.length l2 == 0 || List.length l3 == 0 
    then []
    else [(List.hd l1, List.hd l2, List.hd l3)] @ (zip3 (List.tl l1) (List.tl l2) (List.tl l3))

