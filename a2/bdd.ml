open Formula;;
module BDD = struct
		type node = Node_root of bool
		        | Node_node of string * node * node
		type sat_assignment = string list
		        
		exception Not_implemented

		let stringEquiv n1 = raise Not_implemented ;;
		let stringEquivTuple (n1, n2) = raise Not_implemented ;;
		let hashingnode n1 = raise Not_implemented ;;
		let hashingTuple (n1, n2) = raise Not_implemented ;;
		let nodeEqual (n1, n2) = raise Not_implemented ;;
		let nodeTupleEqual ((n1, n2), (n3, n4)) = raise Not_implemented ;;
		let hashingTriple (s, n1, n2) = raise Not_implemented ;;
		let compareTriple ((s1, n11, n12), (s2, n21, n22)) = raise Not_implemented ;;
		let checkRoot n1 = raise Not_implemented ;;
		let checknode n1 = raise Not_implemented ;;
		let getBool n1 = raise Not_implemented ;;
		let getVar n1 = raise Not_implemented ;;
		let getLow n1 = raise Not_implemented ;;
		let getHigh n1 = raise Not_implemented ;;
		
		let bddFromExpr bexpr order = raise Not_implemented ;;
		
		let sat_count n1 = raise Not_implemented ;;
		let all_sat n1 = raise Not_implemented ;;
		
end  ;;
