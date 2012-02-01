(* DAIMI-Scheme.front-end/2boxer.sml *)
(* a boxer for DAIMI Scheme          *)
(* Olivier Danvy <danvy@brics.dk>    *)
(* October and November 2003         *)

(* ********** *)

structure Syntax2
= struct
    datatype formals = VARIADIC of Identifier.identifier
                     | FIXED of Identifier.identifier list

    datatype expression = INTEGER of int
                        | BOOLEAN of bool
                        | CHARACTER of char
                        | STRING of string
                        | SYMBOL of string
			| NIL
			| IDENTIFIER of Identifier.identifier
                        | LAMBDA of formals * expression
                        | IF of expression * expression * expression
                        | LET of (Identifier.identifier * expression) list * expression
                        | LETSTAR of (Identifier.identifier * expression) list * expression
                        | LETREC of (Identifier.identifier * formals * expression) list * expression
                        | BEGIN of expression * expression list
                        | SETBANG of Identifier.toplevel_identifier * expression
			| APPLICATION of expression * expression list

    datatype definition = DEFINE of Identifier.toplevel_identifier * expression

    datatype program = PROGRAM of definition list * expression
  end;

(* ********** *)

signature SYNTAX2_PARSER
= sig
    val main : Syntax1.program -> Syntax2.program
  end;

structure Syntax2_parser : SYNTAX2_PARSER
= struct
    local open Syntax2
    in 
      exception OMYGOSH
      fun intersperse (nil, nil)
	  = nil
	| intersperse (x :: xs, ys)
	  = if Mutable_variables.occurp_local_mutable x
	    then (case ys
		    of (y :: ys)
		       => y :: (intersperse (xs, ys))
		     | nil
		       => raise OMYGOSH)
	    else x :: (intersperse (xs, ys))
	| intersperse _
	  = raise OMYGOSH
			  
       fun box_expression (Syntax1.INTEGER n)
	   = INTEGER n
	 | box_expression (Syntax1.BOOLEAN b)
	   = BOOLEAN b
	 | box_expression (Syntax1.CHARACTER c)
	   = CHARACTER c
	 | box_expression (Syntax1.STRING s)
	   = STRING s
	 | box_expression (Syntax1.SYMBOL s)
	   = SYMBOL s
	 | box_expression Syntax1.NIL
	   = NIL
	 | box_expression (Syntax1.IDENTIFIER x)
	   = if Mutable_variables.occurp_local_mutable x
	     then APPLICATION (IDENTIFIER "vector-ref",
			       [IDENTIFIER x,
				INTEGER 0])
	     else if Mutable_variables.occurp_toplevel_mutable x
		  then APPLICATION (IDENTIFIER "dereference-toplevel-variable",
				    [IDENTIFIER x])
		  else IDENTIFIER x
	 | box_expression (Syntax1.LAMBDA (Syntax1.VARIADIC x, e))
	   = if Mutable_variables.occurp_local_mutable x
	     then let val x' = Gensym.new (x ^ "%")
		  in LAMBDA (VARIADIC x',
			     LET ([(x, APPLICATION (IDENTIFIER "vector", [IDENTIFIER x']))],
				  box_expression e))
		  end
	     else LAMBDA (VARIADIC x,
			  box_expression e)
	 | box_expression (Syntax1.LAMBDA (Syntax1.FIXED xs, e))
	   = (case List_misc.filter1 (Mutable_variables.occurp_local_mutable, xs)
		of nil
		   => LAMBDA (FIXED xs,
			      box_expression e)
		 | ys
		   => let val ys' = List_misc.map (fn y => Gensym.new (y ^ "%"), ys)
		      in LAMBDA (FIXED (intersperse (xs, ys')),
				 LET (List_misc.map2 (fn (y, y') => (y, APPLICATION (IDENTIFIER "vector", [IDENTIFIER y'])),
						      ys,
						      ys'),
				      box_expression e))
		      end)
	 | box_expression (Syntax1.IF (test, consequent, alternative))
	   = IF (box_expression test, box_expression consequent, box_expression alternative)
	 | box_expression (Syntax1.LET (bindings, body))
	   = LET (List_misc.map (fn (x, e) => if Mutable_variables.occurp_local_mutable x
					      then (x, APPLICATION (IDENTIFIER "vector", [box_expression e]))
					      else (x, box_expression e),
				 bindings),
		  box_expression body)
	 | box_expression (Syntax1.LETSTAR (bindings, body))
	   = LETSTAR (List_misc.map (fn (x, e) => if Mutable_variables.occurp_local_mutable x
						  then (x, APPLICATION (IDENTIFIER "vector", [box_expression e]))
						  else (x, box_expression e),
				     bindings),
		      box_expression body)
	 | box_expression (Syntax1.LETREC (bindings, body))
	   = LETREC (List_misc.map (fn (f, Syntax1.VARIADIC x, e)
				       => if Mutable_variables.occurp_local_mutable x
					  then let val x' = Gensym.new (x ^ "%")
					       in (f, 
						   VARIADIC x',
						   LET ([(x, APPLICATION (IDENTIFIER "vector", [IDENTIFIER x']))],
							box_expression e))
					       end
					  else (f, 
						VARIADIC x,
						box_expression e)
				     | (f, Syntax1.FIXED xs, e)
				       => (case List_misc.filter1 (Mutable_variables.occurp_local_mutable, xs)
					     of nil
						=> (f,
						    FIXED xs,
						    box_expression e)
					      | ys
						=> let val ys' = List_misc.map (fn y => Gensym.new (y ^ "%"), ys)
						   in (f,
						       FIXED (intersperse (xs, ys')),
						       LET (List_misc.map2 (fn (y, y') => (y,
											   APPLICATION (IDENTIFIER "vector",
													[IDENTIFIER y'])),
									    ys,
									    ys'),
							    box_expression e))
						   end),
				    bindings),
		    case List_misc.filter1 (Mutable_variables.occurp_local_mutable,
					    List_misc.map (fn (f, _, _) => f, bindings))
		      of nil
			 => box_expression body
		       | ys
			 => LET (List_misc.map (fn y => (y, APPLICATION (IDENTIFIER "vector", [IDENTIFIER y])),
						ys),
				 box_expression body))
	 | box_expression (Syntax1.BEGIN (e, es))
	   = BEGIN (box_expression e, List_misc.map (box_expression, es))
	 | box_expression (Syntax1.SETBANG (x, e))
	   = if Mutable_variables.occurp_local_mutable x
	     then APPLICATION (IDENTIFIER "vector-set!",
			       [IDENTIFIER x,
				INTEGER 0,
				box_expression e])
	     else SETBANG (x, box_expression e)
	 | box_expression (Syntax1.APPLICATION (e, es))
	   = APPLICATION (box_expression e, List_misc.map (box_expression, es))

       fun box_definition (Syntax1.DEFINE (x, e))
	   = DEFINE (x, box_expression e)

       fun main (Syntax1.PROGRAM (ds, e))
	   = let val ds = List_misc.map (box_definition, ds)
		 val e = box_expression e
	     in PROGRAM (if Mutable_variables.any_mutable_toplevel_variables ()
			 then (let val x = Gensym.new "the-mutable-guy%"
			       in DEFINE ("dereference-toplevel-variable",
					  LAMBDA (FIXED [x], IDENTIFIER x))
			       end) :: ds
			 else ds,
			 e)
	     end
    end
  end;

(* ********** *)

(* end of DAIMI-Scheme.front-end/2boxer.sml *)
