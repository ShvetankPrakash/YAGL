(* Modify this file to produce a word frequency counter.



*)

{
  type token = EOF | Word of string
}

rule token = parse
  | eof { EOF }
  | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
  | _ { token lexbuf }

{

module StringMap = Map.Make(String);;

 let lexbuf = Lexing.from_channel stdin in
 let wordlist = 
   let rec next l =
     match token lexbuf with
	 EOF -> l
     | Word(s) -> next (s :: l)
   in next []
 in

 (* Replace the code below this comment with code that scans through
    the list and builds a string map whose keys are words and whose
    values count the number of apearances of each word.  Then, use
    StringMap.fold to convert this to a list of (count, word) tuples. *)
 
 (* https://riptutorial.com/ocaml/example/11105/recursive-list-processing-with-pattern-matching *)
 let rec myscan wl wm = match wl with
          [] -> wm
     | hd::tl -> if (StringMap.mem hd wm) then
                         myscan tl (StringMap.add hd ((StringMap.find hd wm)+1) wm)
                 else
                         myscan tl (StringMap.add hd 1 wm)
 in
  
 let wordmap = myscan wordlist StringMap.empty in (* List -> Map *)

 let wordcounts = StringMap.fold (fun k v l -> (k, v)::l) wordmap [] in (* Map -> unsorted list of pairs *)

 (* Use the following code to sort the list of (count, word) pairs *)
 let sortedcounts = List.sort (fun (_, c1) (_, c2) -> Pervasives.compare c2 c1) wordcounts in

 (* Print the count, word pairs using List.iter *)
 List.iter (fun (k, v) -> print_int v; print_string " "; print_string k; print_newline ()) sortedcounts

(*
The output from running wordcount on the README file should begin
# ./wordcount < README
12 the
11 to
11 and
8 should
4 for
4 foo
4 be
4 baz
4 bar
4 assignment
4 a
3 zip
...

 *)

}
