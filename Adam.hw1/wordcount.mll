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
 
 List.iter print_endline wordlist

 (* Use the following code to sort the list of (count, word) pairs

 let wordcounts =
   List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) wordcounts in
  *)

 (* Print the count, word pairs using List.iter

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
