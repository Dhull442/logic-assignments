module type Assignment3 = sig
    (* Term variable or k-ary function of terms. 
    A constant can be represented as zero-ary function *)
    type term = V of string | F of string * (term list)
    
    (* Formula is k-ary predicate symbol of terms or quantified conjunction or disjunction of formulae *)
    type form = PRED of string * (term list)
                  | NOT of form
                  | AND of form * form
                  | OR of form * form
                  | FORALL of term * form (* This term should be a variable only*)
                  | EXISTS of term * form (* This term should be a variable only*)
                  
    (* not well-formed formula. Returns the list terms that are not well-formed in the list of formulae *)
    exception Not_wff of (term list * form list)
    
    (* not closed formula. Returns the list of formulae that are not closed *)
    exception Not_closed of formula list		
    
    (* Return true the input formula is well-formed, false otherwise.
    A formula is well-formed if the terms are all well-formed and satisfy the arity constraints *)
    val wff: form -> bool
    
    (* Return the list of free variables in the input formula *)
    val fv: form -> term list (* Term list consists of variables only *)
    
    (* Returns true if a formula is closed, false otherwise *)
    val closed: form -> bool
    
    (* The function takes a list of formulae $\Phi$ (first argument) and a formula $\psi$ (second argument)
    and checks if $\psi$ is a logical consequence of $\Phi$ by generating tableau
    This function should raise appropriate exceptions and output a dot file named 'tableau.dot'. *)
    val create_tableau: form list -> form -> unit
end;;
