let array_rev arr =
let n = Array.length arr in
for i=0 to n/2-1 do
  let temp = arr.(i) in
  arr.(i) <- arr.(n-1-i);
  arr.(n-1-i) <- temp;
done;
arr
;;
(* val array_rev : 'a array -> unit

   Reverses the given array in place. Uses iteration and mutation to
   do so efficiently. DOES NOT generate any internal copies of the
   parameter array.

   REPL EXAMPLES:
   # let a1 = [|1; 2; 3;|];;
   val a1 : int array = [|1; 2; 3|]
   # array_rev a1;;
   - : unit = ()
   # a1;;
   - : int array = [|3; 2; 1|]
   # let a2 = [|"a"; "b"; "c"; "d"; "e"; "f"|];;
   val a2 : string array = [|"a"; "b"; "c"; "d"; "e"; "f"|]
   # array_rev a2;;
   - : unit = ()
   # a2;;
   - : string array = [|"f"; "e"; "d"; "c"; "b"; "a"|]
   # let a3 = [|true; true; false; false; true;|];;
   val a3 : bool array = [|true; true; false; false; true|]
   # array_rev a3;;
   - : unit = ()
   # a3;;
   - : bool array = [|true; false; false; true; true|]
*)
let list_rev lst =
  if lst=[] then
    lst
  else
    let rec helper ori rev =
      let hd = List.hd ori in
      let tl = List.tl ori in
      if tl = [] then
        hd::rev
      else
        helper tl (hd::rev)
    in
    helper lst []
  ;;
(* val list_rev : 'a list -> 'a list

   Return a reversed copy of the given list. Does not (and cannot)
   modify the original list. Uses an internal recursive function to
   build the reversed list. The internal function is tail-recursive.

   REPL EXAMPLES:
   # list_rev lst1;;
   - : int list = [3; 2; 1]
   # lst1;;
   - : int list = [1; 2; 3]
   # let lst2 = ["a"; "b"; "c"; "d"; "e"; "f"];;
   val lst2 : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # list_rev lst2;;
   - : string list = ["f"; "e"; "d"; "c"; "b"; "a"]
   # lst2;;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # let lst3 = [true; true; false; false; true];;
   val lst3 : bool list = [true; true; false; false; true]
   # list_rev lst3;;
   - : bool list = [true; false; false; true; true]
   # lst3;;
   - : bool list = [true; true; false; false; true]
*)
