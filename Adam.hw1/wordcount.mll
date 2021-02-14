(* Modify this file to produce a word frequency counter. *)

{
  type token = EOF | Word of string
}

rule token = parse
  | eof { EOF }
  | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
  | _ { token lexbuf }

{

module StringMap = Map.Make(String);;

let incrementStringMap (m) (w) = match StringMap.mem w m with
	| false -> StringMap.add w 1 m
	| true -> StringMap.add w (StringMap.find w m + 1) m;;


let rec print_tuples =
  function
  | [] -> ()
  | (a, b) :: rest ->
    Printf.printf "%i %s\n" a b;
    print_tuples rest;;


 let lexbuf = Lexing.from_channel stdin in
 let wordlist = 
   let rec next l =
     match token lexbuf with
	 EOF -> l
     | Word(s) -> next (s :: l)
   in next []
 in
 
let wordcountmap = List.fold_left (fun s e -> incrementStringMap s e) StringMap.empty wordlist in
let wordcounts = StringMap.fold
	(fun k v l -> (v, k) :: l) wordcountmap []
in

let wordcounts =
    List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) wordcounts in

print_tuples wordcounts

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
