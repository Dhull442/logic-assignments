open Formula;;
open Bdd;;
open Program;;
open BDD;;

(* 	The type of n_queen is int -> string list, where int is the
	size of board. The solution is represented as list of strings,
	where each string denotes the position of a queen in the solution.
	The position is a string 'c' (lower case c without quotes)
	appended with two single digit integers i and j, where i and j
	are row and column numbers respectively, starting from 0.
	For example, the string for cell in row 0 and column 4 should be c04.
*)
exception OutOfBound
let rec type1 i j n l =
	if l != i then
		if l == n-1 then (OprUnary (NOT,Variable ("c"^string_of_int(l)^string_of_int(j))))
		else if l > n-1 then Constant true
		else (OprBinary (AND, OprUnary (NOT,Variable ("c"^string_of_int(l)^string_of_int(j))), type1 i j n (l+1)))
	else type1 i j n (l+1);;

let rec type2 i j n l =
	if l != j then
		if l == n-1 then (OprUnary (NOT,Variable ("c"^string_of_int(i)^string_of_int(l))))
		else if l > n-1 then Constant true
		else (OprBinary (AND, OprUnary (NOT,Variable ("c"^string_of_int(i)^string_of_int(l))), type2 i j n (l+1)))
	else type2 i j n (l+1);;

let rec type12 i j n =
	if i < n then OprBinary (AND, OprBinary (IFTHEN, Variable ("c"^string_of_int(i)^string_of_int(j)), OprBinary (AND, type1 i j n 0, type2 i j n 0)), type12 (i+1) j n)
	else (if j < n-1 then type12 0 (j+1) n
	else (Program.Constant true))
;;
let linecst n = type12 0 0 (n);;
let rec type3 i j n k =
	if k != i then
		if (k < n && k < (n - (j-i)) && (j+k-i)>=0) then OprBinary (AND, OprUnary (NOT, Variable ("c"^string_of_int(k)^string_of_int(j+k-i))), type3 i j n (k+1))
		else if (j+k-i) < 0 then type3 i j n (k+1)
		else Program.Constant true
	else type3 i j n (k+1);;
let rec type4 i j n k =
	if k != i then
		if (k < n && (j+i -k) >=0 && (j+i -k)<n) then OprBinary (AND, OprUnary (NOT, Variable ("c"^string_of_int(k)^string_of_int(j+i-k))), type4 i j n (k+1))
		else if (j+i-k) >= n then type4 i j n (k+1)
		else Program.Constant true
	else type4 i j n (k+1);;
let rec type34 i j n =
	if i < n then OprBinary (AND, OprBinary (IFTHEN, Variable ("c"^string_of_int(i)^string_of_int(j)), OprBinary (AND, type3 i j n 0, type4 i j n 0)), type34 (i+1) j n)
	else (if j < n-1 then type34 0 (j+1) n
	else (Program.Constant true))
;;
let rec type5 i j n =
	if i < n then OprBinary (OR, Variable ("c"^string_of_int(i)^string_of_int(j)), type5 (i+1) j n)
	else (Constant false);;
let rec type6 j n =
	if j < n then OprBinary (AND,type5 0 j n,type6 (j+1) n)
	else Constant true;;
let diagcst n = type34 0 0 n ;;
let presentcst n = type6 0 n;;
let allcst n =
let e1 = linecst n and e2 = diagcst n and e3 = presentcst n in
OprBinary (AND,(OprBinary (AND, e1, e2)),e3);;
let varlist n =
	let rec oneside n = if(n==0) then ["c0"] else ("c"^string_of_int(n)) :: (oneside (n-1)) in
	let l = oneside (n-1) in
	let rec fillall l1 n l2 l3 = match l1 with | li :: ls -> (fillall ls n ((li^string_of_int(n)) :: l2) l3) | [] -> if n == 0 then l2 else fillall l3 (n-1) l2 l3 in
	fillall l (n-1) [] l
;;
let n_queen_bdd n = BDD.bddFromExpr (allcst (n)) (varlist n);;
let n_queen n = BDD.any_sat (n_queen_bdd n);;

(*	The type of  knight is int -> int -> int -> string list, where
	first int is board size, second and third ints represent the
	row and column number (starting from 0) respectively of
	initial position of the knight on the board
	The output to this function should be a sequence of strings.
	The first element in the sequence should be cell name
	corresponding to the initial position of the knight.
	Each subsequent string in the sequence should represent the
	next cell visited by the knight.
*)
let knight board_size init_row init_col = raise BDD.Not_implemented ;;

(*
- Q - -
- - - Q
Q - - -
- - Q -

- - Q - -
- - - - Q
- Q - - -
- - - Q -
Q - - - -
*)

let rec printlist l = match l with | li :: ls -> print_string(li^" ");printlist ls; | [] -> print_string("\n");;

(* Tests
let r = n_queen_bdd (6);;
printlist (any_sat r);;
print_string(string_of_int(sat_count r)^"\n");;
to_dot r ;;*)
