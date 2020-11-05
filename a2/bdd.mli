open Formula;;
module BDD : sig
		type node = Node_root of bool
		        | Node_node of string * node * node
		type sat_assignment = string list
		
		exception Not_implemented

		val stringEquiv : node -> string
		val stringEquivTuple : (node * node) -> string
		val hashingnode : node -> int
		val hashingTuple : (node * node) -> int
		val nodeEqual : (node * node) -> bool
		val nodeTupleEqual : (node * node) * (node * node) -> bool
		val hashingTriple : string * node * node -> int
		val compareTriple : (string * node * node) * (string * node * node) -> bool
		val checkRoot : node -> bool
		val checknode : node -> bool
		val getBool : node -> bool
		val getVar : node -> string
		val getLow : node -> node
		val getHigh : node -> node
		
		val bddFromExpr : Program.bool_expr -> string list -> node 
		
		val sat_count: node-> int
		val all_sat: node -> sat_assignment list
		
end ;;
