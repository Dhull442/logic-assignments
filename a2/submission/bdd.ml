open Formula;;
open Program;;
open Printf;;

module IntMap = Map.Make(struct type t = int let compare = compare end);;
let comparet (x1,x2,x3) (y1,y2,y3) = let k1 = compare x1 y1 and k2 = compare x2 y2 and k3 = compare x3 y3 in if(k1!=0) then k1 else if(k2!=0) then k2 else if(k3!=0) then k3 else 0;;
module RMap = Map.Make(struct type t = string*int*int	let compare = comparet end);;
module BDD = struct
type sat_assignment = string list
(* define the type robdd in whatever way you want  *)
type robdd = Robdd of int * ((string * int * int) IntMap.t) * int ;;

let initT = IntMap.empty;;
let initT = IntMap.add 0 ("False",-1,-1) initT;;
let initT = IntMap.add 1 ("True",-1,-1) initT;;

let initH = RMap.empty;;

exception Not_implemented
exception Incorrect_flow

(* return (node, T, H) for a (T, H, literal, lower node, higher node) *)
let mk t h literal lo hi =
	if lo == hi then (lo,t,h)
	else if RMap.mem (literal,lo,hi) h then
		(RMap.find (literal,lo,hi) h, t, h)
	else
		let u = IntMap.cardinal t in
			let t = IntMap.add u (literal,lo,hi) t
			and h = RMap.add (literal,lo,hi) u h
				in (u,t,h)
;;

(* Simplifies the boolean expr if possible *)
let simplify bexpr =
	match bexpr with
	| (Program.OprUnary (opr, (Program.Constant bval))) -> (Program.Constant (not bval))
	| (Program.OprBinary (opr , expr1 , expr2)) ->(
		match (expr1, expr2) with
		| (Program.Constant bval1, Program.Constant bval2) ->	(
			match opr with
			| Program.AND -> (Program.Constant (bval1 && bval2))
			| Program.OR -> (Program.Constant (bval1 || bval2))
			| Program.IFTHEN -> if bval1 then (Program.Constant bval2) else (Program.Constant true)
			| Program.IFF -> (Program.Constant (bval1 == bval2))
			| _ -> bexpr
			)
		| (Program.Constant bval1, _ ) -> (
			match opr with
			| Program.AND -> if bval1 then expr2 else (Program.Constant false)
			| Program.OR -> if bval1 then (Program.Constant true) else expr2
			| Program.IFTHEN -> if bval1 then expr2 else (Program.Constant true)
			| _ -> bexpr
			)
		| (_,Program.Constant bval2) -> (
			match opr with
			| Program.AND -> if bval2 then expr1 else (Program.Constant false)
			| Program.OR -> if bval2 then (Program.Constant true) else expr1
			| Program.IFTHEN -> if bval2 then Program.Constant true else Program.OprUnary (Program.NOT, expr1)
			| _ -> bexpr
			)
		| _ -> bexpr
		)
	| (Program.OprTernary (opr,Program.Constant bval,expr2,expr3)) -> if bval then expr2 else expr3
	| _ -> bexpr
;;

(* Recursive substitution of literal with value(Program const)*)
let rec subs bexpr l value =
	match bexpr with
	| (Program.Variable l1) -> if (compare l1 l == 0) then value else bexpr
	| (Program.Constant _) -> bexpr
	| (Program.OprUnary (opr, expr)) -> let nexpr = subs expr l value in simplify (Program.OprUnary (opr,nexpr))
	| (Program.OprBinary (opr, expr1, expr2)) -> let nexpr1 = subs expr1 l value and nexpr2 = subs expr2 l value in simplify (Program.OprBinary (opr, nexpr1, nexpr2))
	| (Program.OprTernary (opr, expr1, expr2, expr3)) -> let nexpr1 = subs expr1 l value and nexpr2 = subs expr2 l value and nexpr3 = subs expr3 l value in simplify (Program.OprTernary (opr, nexpr1, nexpr2, nexpr3))
;;
let rec fsimp bexpr =
	match bexpr with
	| Program.OprUnary (opr,expr1) -> simplify (Program.OprUnary (opr,fsimp expr1))
	| Program.OprBinary (opr,expr1,expr2) -> simplify (Program.OprBinary (opr, fsimp expr1,fsimp expr2))
	| Program.OprTernary (opr, e1,e2,e3) -> simplify (Program.OprTernary (opr, fsimp e1, fsimp e2, fsimp e3))
	| Program.Constant _ -> bexpr
	| Program.Variable _ -> bexpr
;;
exception Didntunderstand
let rec build bexpr order t h =
	match order with
	| [] -> (let bn = fsimp bexpr in
		match bn with
		| (Program.Constant bval) -> if bval then (1,t,h) else (0,t,h)
		| _ -> raise Didntunderstand
		)
	| l :: ls -> let 	(v0,t0,h0) = build (subs bexpr l (Program.Constant false)) ls t h in
								let (v1,t1,h1) = build (subs bexpr l (Program.Constant true )) ls t0 h0 in
										(mk t1 h1 l v0 v1)
;;

let bddFromExpr bexpr order =
	let n = List.length order in
	let t = initT and h = initH in
	let (u,tf,_) = build bexpr order t h in
	Robdd (u,tf,n)
;;

let rec pow a n = if n <= 0 then 1 else a*(pow a (n-1));;

let rec count u t dp =
if (IntMap.mem u dp) then ((IntMap.find u dp),dp)
else	let (l,low,hi) = (IntMap.find u t) in
			let (lc,dp1) = count low t dp in
			let (hc,dp2) = count hi t dp1 in
			let cu = (lc + hc)/2 in
			let dpF = IntMap.add u cu dp2 in
			(cu,dpF)
;;

(* SAT COUNT *)
let sat_count bdd =
let dp = IntMap.empty in
let dp = IntMap.add 0 0 dp in
 	match bdd with
	| Robdd (u,t,n) -> let dp = IntMap.add 1 (pow 2 n) dp in let (cu,dpf) = (count u t dp) in cu
;;

(* ALL SAT *)
let rec prefix lit l =
	match l with
	| li :: ls -> (lit :: li) :: (prefix lit ls)
	| [] -> []
;;
let rec allsat u t =
	if u == 0 then []
	else if u == 1 then [[]]
	else let (l,lo,hi) = (IntMap.find u t) in
	let l1 = allsat lo t and l2 = prefix l (allsat hi t) in (List.append l1 l2)
;;
let all_sat bdd =
 	match bdd with
	| Robdd (u,t,_) -> allsat u t
;;

exception Not_Satisfied

let rec find_sat u t =
	if u == 0 then raise Not_Satisfied
	else if u == 1 then []
	else let (l,lo,hi) = (IntMap.find u t) in
	 	if lo == 0 then l :: (find_sat hi t)
		else (find_sat lo t)
;;

(* ANY SAT *)
let any_sat bdd =
	match bdd with
	| Robdd (u,t,_) -> if u == 0 then [""] else find_sat u t
;;


(*TO DOT*)
let rec dotlist u t ls=
	if u >= (IntMap.cardinal t) then ls
	else let (l,lo,hi) = (IntMap.find u t) in
	let (lol,_,_) = (IntMap.find lo t) and (lhi,_,_) = (IntMap.find hi t) in
	dotlist (u+1) t ((l^": "^(string_of_int u),lol^": "^(string_of_int lo),-1) :: ((l^": "^(string_of_int u),lhi^": "^(string_of_int hi),1) :: ls))
;;
let rec printall l oc =
	match l with
	| (a,b,c) :: ls -> if c > 0 then (fprintf oc "\"%s\" -> \"%s\" \n" a b) else (fprintf oc "\"%s\" -> \"%s\" [style=dotted] \n" a b)  ; printall ls oc;
	| [] -> fprintf oc "";
;;
let to_dot bdd =
	match bdd with
	| Robdd (u,t,_) -> let l = dotlist 2 t [] in
		let oc = open_out "bdd.dot" in
		fprintf oc "digraph BDD { \n"; printall l oc; fprintf oc "} \n"; close_out oc;
;;

let num_nodes bdd =
match bdd with
| Robdd (_,t,_) -> IntMap.cardinal t;;
end  ;;


(* Testing  #use "bdd.ml";;
let bexpr1 = (Program.OprBinary (OR, Program.OprBinary (IFF, Program.Variable "x1", Program.Variable "x2"),Program.Variable "x3"));;
let	order1 = ["x1";"x2";"x3"];;
let r1 = BDD.bddFromExpr bexpr1 order1;;
BDD.num_nodes r1;;
let t1 = BDD.subs bexpr1 "x1" (Program.Constant true);;
let t2 = BDD.subs t1 "x2" (Program.Constant true);;
let a = (Program.Constant true);;
let exp = OprBinary (AND,OprBinary (OR, Constant false, Constant true),Constant false);;
a;; *)
