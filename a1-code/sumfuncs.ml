let array_sum arr =

let n = Array.length arr in
let sum = ref 0 in
for i=0 to n-1 do
  let next = !sum + arr.(i) in
  sum := next;
done;
!sum
;;
(* val array_sum : int array -> int
   Return the sum of int array arr. Uses Array.length to calculate its
   length. Uses a ref to a summing int and a loop over the array
   elements.

   REPL EXAMPLES:
   # array_sum [|1; 3; 5|];;
   - : int = 9
   # array_sum [|4; -3; 12; 2|];;
   - : int = 15
   # array_sum [||];;
   - : int = 0
*)

let rec list_sum lst =
  if lst = [] then
     0
  else
    let h = List.hd lst in
    let n = List.tl lst in
    let tl = list_sum n in
    h + tl ;;

(*
  function sum(A)
    if sum is empty:
      return 0
    else
      return sum = A[0]+ function sum(A[1:length])

    let List.hd = A.(0)
    let list.tl = list sum in
*)
(* val list_sum : int list -> int
   Return the sum of int list lst. Uses recursion and NO mutation.
   Uses List.hd and List.tl to get the head and tail of a list.

   REPL EXAMPLES:
   # list_sum [1; 3; 5];;
   - : int = 9
   # list_sum [4; -3; 12; 2];;
   - : int = 15
   # list_sum [];;
   - : int = 0
*)
