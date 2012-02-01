(* DAIMI-Scheme.front-end/1desugarer.sml *)
(* a desugarer for DAIMI Scheme          *)
(* Olivier Danvy <danvy@brics.dk>        *)
(* October and November 2003             *)

(* ********** *)

structure Syntax1
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
                        | SETBANG of Identifier.identifier * expression
			| APPLICATION of expression * expression list

    datatype definition = DEFINE of Identifier.toplevel_identifier * expression

    datatype program = PROGRAM of definition list * expression
  end;

(* ********** *)

structure List_misc
= struct
    fun map_append (f, xs)
	= let fun walk nil
		  = nil
		| walk (x :: xs)
		  = (f x) @ (walk xs)
	  in walk xs
	  end

    fun map (f, xs)
	= List.map f xs

    exception LISTS_NOT_OF_SAME_LENGTH

    fun map2 (f, xs, ys)
	= let fun walk (nil, nil)
		  = nil
		| walk (x :: xs, y :: ys)
		  = (f (x, y)) :: (walk (xs, ys))
		| walk _
		  = raise LISTS_NOT_OF_SAME_LENGTH
	  in walk (xs, ys)
	  end

    fun filter1 (p, xs)
	= let fun walk nil
		  = nil
		| walk (x :: xs)
		  = if p x
		    then x :: (walk xs)
		    else walk xs
	  in walk xs
	  end
  end;

(* ********** *)

signature ENV
= sig
    type 'a env
    val empty : 'a env
    val extend : Identifier.identifier * 'a * 'a env -> 'a env
    val extendlis : Identifier.identifier list * 'a list * 'a env -> 'a env
    val lookup : Identifier.identifier * 'a env -> 'a option
  end;

structure Env : ENV
= struct
    type 'a env = (Identifier.identifier * 'a) list

    val empty = nil

    fun extend (x, d, env)
	= (x, d) :: env

    exception INTERNAL_MISMATCH

    fun extendlis (nil, nil, env)
	= env
      | extendlis (x :: xs, v :: vs, env)
	= (x, v) :: (extendlis (xs, vs, env))
      | extendlis _
	= raise INTERNAL_MISMATCH

    fun lookup (x, env)
	= let fun walk nil
		  = NONE
		| walk ((x', d) :: env)
		  = if x = x'
		    then SOME d
		    else walk env
	  in walk env
	  end
  end;

(* ********** *)

structure Balanced_tree
= struct
    datatype bt = NOTHING
		| FORK of bt ref * string * bt ref

    fun insert (x, t)
	= let fun walk (r as (ref NOTHING))
		  = (r := FORK (ref NOTHING, x, ref NOTHING))
		| walk (ref (FORK (left, x', right)))
		  = (case String.compare (x, x')
		       of General.LESS
			 => walk left
			| General.EQUAL
			 => ()
			| General.GREATER
			 => walk right)
	  in walk t
	  end

    fun insert_list (xs, t)
	= let fun walk nil
		  = ()
		| walk (x :: xs)
		  = (insert (x, t);
		     walk xs)
	  in walk xs
	  end

    fun occurp (x, t)
	= let fun walk NOTHING
		  = false
		| walk (FORK (ref left, x', ref right))
		  = (case String.compare (x, x')
		       of General.LESS
			  => walk left
			| General.EQUAL
			  => true
			| General.GREATER
			  => walk right)
	  in walk (!t)
	  end
  end;

(* ********** *)


signature MUTABLE_VARIABLES
= sig
    val predefinedp : Identifier.identifier -> bool
    val globalp : Identifier.identifier -> bool
    val init : Identifier.toplevel_identifier list -> unit
    val insert_local_mutable : Identifier.identifier -> unit
    val occurp_local_mutable : string -> bool
    val insert_toplevel_mutable : Identifier.identifier -> unit
    val occurp_toplevel_mutable : string -> bool
    val insert_letrec_mutables : Identifier.identifier list -> unit
    val occurp_letrec_mutable : string -> bool
    val any_mutable_toplevel_variables : unit -> bool
  end;

structure Mutable_variables : MUTABLE_VARIABLES
= struct
    val hook_predefined = ref Balanced_tree.NOTHING

    val _ = Balanced_tree.insert_list (["integer?",
					"+",
                                        "-",
                                        "*",
                                        "quotient",
                                        "remainder",
                                        "<",
                                        "<=",
                                        "=",
                                        ">=",
                                        ">",
                                        "boolean?",
                                        "symbol?",
                                        "char?",
                                        "char->integer",
                                        "integer->char",
                                        "string",
                                        "make-string",
                                        "string?",
                                        "string-length",
                                        "string-append",
                                        "string=?",
                                        "string-ref",
                                        "string->symbol",
                                        "symbol->string",
                                        "pair?",
                                        "cons",
                                        "car",
                                        "cdr",
                                        "set-car!",
                                        "set-cdr!",
                                        "null?",
                                        "vector",
                                        "make-vector",
                                        "vector?",
                                        "vector-length",
                                        "vector-ref",
                                        "vector-set!",
                                        "procedure?",
                                        "apply",
                                        "eqv?",
                                        "call/cc",
                                        "exit",
                                        "open-input-file",
                                        "input-port?",
                                        "close-input-port",
                                        "current-input-port",
                                        "read-char",
                                        "peek-char",
                                        "eof-object?",
                                        "open-output-file",
                                        "output-port?",
                                        "close-output-port",
                                        "current-output-port",
                                        "write-char"], hook_predefined)

    fun predefinedp x
	= Balanced_tree.occurp (x, hook_predefined)

    (* ********** *)

    val hook_global = ref Balanced_tree.NOTHING

    fun globalp x
	= Balanced_tree.occurp (x, hook_global)

    (* ********** *)

    val hook_local_mutable = ref Balanced_tree.NOTHING

    val hook_toplevel_mutable : Identifier.toplevel_identifier list ref = ref nil

    val hook_letrec_mutable : Identifier.identifier list ref = ref nil

    (* ********** *)

    fun init global_names
	= (hook_global := Balanced_tree.NOTHING;
	   Balanced_tree.insert_list (global_names, hook_global);
	   hook_local_mutable := Balanced_tree.NOTHING;
	   hook_toplevel_mutable := nil)

    fun insert_local_mutable x
	= Balanced_tree.insert (x, hook_local_mutable)

    fun occurp_local_mutable x
	= Balanced_tree.occurp (x, hook_local_mutable)

    fun insert_toplevel_mutable x
	= if List_utilities.occurp (x, !hook_toplevel_mutable)
	  then ()
	  else (hook_toplevel_mutable := (x :: (!hook_toplevel_mutable)))

    fun insert_letrec_mutables nil
	= ()
      | insert_letrec_mutables (x :: xs)
	= (if List_utilities.occurp (x, !hook_letrec_mutable)
	   then ()
	   else (hook_letrec_mutable := (x :: (!hook_letrec_mutable)));
	   insert_letrec_mutables xs)

    fun occurp_toplevel_mutable x
	= List_utilities.occurp (x, !hook_toplevel_mutable)

    fun occurp_letrec_mutable x
	= List_utilities.occurp (x, !hook_letrec_mutable)

    fun any_mutable_toplevel_variables ()
	= not ((!hook_toplevel_mutable) = nil)
  end;

(* ********** *)

signature SYNTAX1_PARSER
= sig
    val main : Syntax0.program -> Syntax1.program
  end;

structure Syntax1_parser : SYNTAX1_PARSER
= struct
    local open Syntax1
    in 
       fun transmogrify (Sexpr.INTEGER n)
	   = INTEGER n
	 | transmogrify (Sexpr.BOOLEAN b)
	   = BOOLEAN b
	 | transmogrify (Sexpr.CHARACTER c)
	   = CHARACTER c
	 | transmogrify (Sexpr.STRING s)
	   = STRING s
	 | transmogrify (Sexpr.SYMBOL s)
	   = SYMBOL s
	 | transmogrify Sexpr.NIL
	   = NIL
	 | transmogrify (Sexpr.PAIR (e1, e2))
	   = APPLICATION (IDENTIFIER "cons", [transmogrify e1, transmogrify e2])

       fun make_IF (INTEGER _, consequent, alternative)
	   = consequent
	 | make_IF (BOOLEAN false, consequent, alternative)
	   = alternative
	 | make_IF (BOOLEAN true, consequent, alternative)
	   = consequent
	 | make_IF (CHARACTER _, consequent, alternative)
	   = consequent
	 | make_IF (STRING _, consequent, alternative)
	   = consequent
	 | make_IF (SYMBOL _, consequent, alternative)
	   = consequent
	 | make_IF (NIL, consequent, alternative)
	   = consequent
	 | make_IF (LAMBDA _, consequent, alternative)
	   = consequent
	 | make_IF (test, consequent, alternative)
	   = IF (test, consequent, alternative)

       fun simple_expressionp (IDENTIFIER _)
	   = true
	 | simple_expressionp (INTEGER _)
	   = true
	 | simple_expressionp (BOOLEAN _)
	   = true
	 | simple_expressionp (CHARACTER _)
	   = true
	 | simple_expressionp (STRING _)
	   = true
	 | simple_expressionp (SYMBOL _)
	   = true
	 | simple_expressionp NIL
	   = true
	 | simple_expressionp _
	   = false

       fun zap_simple_datum (Syntax0.SIMPLE_INTEGER n)
	   = INTEGER n
	 | zap_simple_datum (Syntax0.SIMPLE_BOOLEAN b)
	   = BOOLEAN b
	 | zap_simple_datum (Syntax0.SIMPLE_CHARACTER c)
	   = CHARACTER c
	 | zap_simple_datum (Syntax0.SIMPLE_SYMBOL s)
	   = SYMBOL s

       exception MUMBLE_BEGIN

       fun flatten_begin (e, es)
	   = let fun walk (nil, a)
		     = a
		   | walk ((Syntax0.BEGIN (e', es')) :: es, a)
		     = walk (e' :: es', walk (es, a))
		   | walk (e :: es, a)
		     = e :: (walk (es, a))
		 fun simple_expressionp (Syntax0.IDENTIFIER _)
		     = true
		   | simple_expressionp (Syntax0.INTEGER _)
		     = true
		   | simple_expressionp (Syntax0.BOOLEAN _)
		     = true
		   | simple_expressionp (Syntax0.CHARACTER _)
		     = true
		   | simple_expressionp (Syntax0.STRING _)
		     = true
		   | simple_expressionp (Syntax0.QUOTATION _)
		     = true
		   | simple_expressionp (Syntax0.LAMBDA _)
		     = true
		   | simple_expressionp _
		     = false
		 fun simplify (e, nil)
		     = [e]
		   | simplify (e, e' :: es)
		     = if simple_expressionp e
		       then simplify (e', es)
		       else e :: simplify (e', es)
	     in case walk (e :: es, nil)
		  of (e' :: es')
		     => (case simplify (e', es')
			   of (e'' :: es'')
			      => (e'', es'')
			    | _
			      => raise MUMBLE_BEGIN)
		   | _
		     => raise MUMBLE_BEGIN
	     end

       exception MUMBLE_LETREC
       exception MUMBLE_ENVIRONMENT
       exception UNDECLARED_VARIABLE
       exception THOU_SHALL_NOT_SETBANG_A_LETREC_DECLARED_VARIABLE

       fun desugar_expression e
	   = let fun walk (Syntax0.INTEGER n, is, env)
		     = (is, INTEGER n)
		   | walk (Syntax0.BOOLEAN b, is, env)
		     = (is, BOOLEAN b)
		   | walk (Syntax0.CHARACTER c, is, env)
		     = (is, CHARACTER c)
		   | walk (Syntax0.STRING s, is, env)
		     = (is, STRING s)
		   | walk (Syntax0.IDENTIFIER x, is, env)
		     = (is, case Env.lookup (x, env)
			      of (SOME y)
				 => y
			       | NONE
				 => if (Mutable_variables.globalp x) orelse
				       (Mutable_variables.predefinedp x)
				    then IDENTIFIER x
				    else (TextIO.output (TextIO.stdOut, "Undeclared variable: ");
					  TextIO.output (TextIO.stdOut, x);
					  raise UNDECLARED_VARIABLE))
		   | walk (Syntax0.QUOTATION e, is, env)
		     = (case transmogrify e
			  of (q as (APPLICATION _))
			     => let val i = Gensym.new "immutable%"
				in ((DEFINE (i, q)) :: is, IDENTIFIER i)
				end
			   | q
			     => (is, q))
		   | walk (Syntax0.LAMBDA (Syntax0.VARIADIC x, e), is, env)
		     = let val x' = Gensym.new (x ^ "%")
			   val (is', e') = walk (e, is, Env.extend (x, IDENTIFIER x', env))
		       in (is', LAMBDA (VARIADIC x', e'))
		       end
		   | walk (Syntax0.LAMBDA (Syntax0.FIXED xs, e), is, env)
		     = let val xs' = List_misc.map (fn x => Gensym.new (x ^ "%"), xs)
			   val (is', e') = walk (e, is, Env.extendlis (xs, List_misc.map (IDENTIFIER, xs'), env))
		       in (is', LAMBDA (FIXED xs', e'))
		       end
		   | walk (Syntax0.IF (test, consequent, alternative), is, env)
		     = let val (is, test') = walk (test, is, env)
			   val (is, consequent') = walk (consequent, is, env)
			   val (is, alternative') = walk (alternative, is, env)
		       in (is, IF (test', consequent', alternative'))
		       end
		   | walk (Syntax0.COND (clauses, alternative), is, env)
		     = let fun traverse (nil, is, env)
			       = walk (alternative, is, env)
			     | traverse ((test, consequent) :: clauses, is, env)
			       = let val (is, test') = walk (test, is, env)
				     val (is, consequent') = walk (consequent, is, env)
				     val (is, alternative') = traverse (clauses, is, env)
				 in (is, make_IF (test', consequent', alternative'))
				 end
		       in traverse (clauses, is, env)
		       end
		   | walk (Syntax0.CASE (test, clauses, alternative), is, env)
		     = let val (is, test') = walk (test, is, env)
			   val process
			       = fn (test', is, env)
				    => let fun traverse (nil, is, env)
				               = walk (alternative, is, env)
					     | traverse ((nil, consequent) :: clauses, is, env)
					       = traverse (clauses, is, env)
					     | traverse ((datum :: data, consequent) :: clauses, is, env)
					       = let val (is, consequent') = walk (consequent, is, env)
						     val (is, alternative') = traverse (clauses, is, env)
						 in (is, IF (let fun loop (datum, nil)
						                     = APPLICATION (IDENTIFIER "eqv?",
										    [test', zap_simple_datum datum])
								   | loop (datum, datum' :: data)
								     = IF (APPLICATION (IDENTIFIER "eqv?",
											[test', zap_simple_datum datum]),
									   BOOLEAN true,
									   loop (datum', data))
							     in loop (datum, data)
							     end,
							     consequent',
							     alternative'))
						 end
				       in traverse (clauses, is, env)
				       end
		       in if simple_expressionp test'
			  then process (test', is, env)
			  else let val x = Gensym.new "case%"
				   val (is, body) = process (IDENTIFIER x, is, env)
			       in (is, LET ([(x, test')], body))
			       end
		       end
		   | walk (Syntax0.CASE_RECORD (test, clauses, alternative), is, env)
		     = let val (is, test') = walk (test, is, env)
			   val process
			       = fn (test', is, env)
				    => let fun traverse (nil, is, env)
				               = walk (alternative, is, env)
					     | traverse (((name, fields), consequent) :: clauses, is, env)
					       = let val fresh_fields = List_misc.map (fn field => Gensym.new (field ^ "_"),
										       fields)
						     val (is, consequent') = walk (consequent,
										   is,
										   Env.extendlis (fields,
												  List_misc.map (IDENTIFIER,
														 fresh_fields),
												  env))
						     val (is, alternative') = traverse (clauses, is, env)
						 in (is, IF (APPLICATION (IDENTIFIER ("is-" ^ name ^ "?"),
									  [test']),
							     LET (List_misc.map2 (fn p => p,
										  fresh_fields,
										  let fun loop (nil, offset)
										          = nil
											| loop (field :: fields, offset)
											  = (APPLICATION (IDENTIFIER "vector-ref",
													  [test',
													   INTEGER offset]))
										  :: (loop (fields, offset + 1))
										  in loop (fields, 1)
										  end),
							          consequent'),
							     alternative'))
						 end
				       in traverse (clauses, is, env)
				       end
		       in if simple_expressionp test'
			  then process (test', is, env)
			  else let val x = Gensym.new "case-record%"
				   val (is, body) = process (IDENTIFIER x, is, env)
			       in (is, LET ([(x, test')], body))
			       end
		       end
		   | walk (Syntax0.AND nil, is, env)
		     = (is, BOOLEAN false)
		   | walk (Syntax0.AND (e :: es), is, env)
		     = let fun traverse (e, nil, is, env)
			       = walk (e, is, env)
			     | traverse (e, e1 :: es, is, env)
			       = let val (is, e') = walk (e, is, env)
				     val (is, consequent) = traverse (e1, es, is, env)
				 in (is, make_IF (e', consequent, BOOLEAN false))
				 end
		       in traverse (e, es, is, env)
		       end
		   | walk (Syntax0.OR nil, is, env)
		     = (is, BOOLEAN true)
		   | walk (Syntax0.OR (e :: es), is, env)
		     = let fun traverse (e, nil, is, env)
			       = walk (e, is, env)
			     | traverse (e, e1 :: es, is, env)
			       = let val (is, e') = walk (e, is, env)
				     val (is, alternative) = traverse (e1, es, is, env)
				 in (is, if simple_expressionp e'
					 then make_IF (e', e', alternative)
					 else let val new = Gensym.new "or%"
					      in LET ([(new, e')],
						      make_IF (IDENTIFIER new,
							       IDENTIFIER new,
							       alternative))
					      end)
				 end
		       in traverse (e, es, is, env)
		       end
		   | walk (Syntax0.LET (nil, body), is, env)
		     = walk (body, is, env)
		   | walk (Syntax0.LET (bindings, body), is, env)
		     = let fun traverse (nil, is, env)
			       = (is, nil, nil, nil)
			     | traverse ((x, e) :: bindings, is, env)
			       = let val x' = Gensym.new (x ^ "%")
				     val (is, e') = walk (e, is, env)
				     val (is, bindings', xs, xs') = traverse (bindings, is, env)
				 in (is, (x', e') :: bindings', x :: xs, (IDENTIFIER x') :: xs')
				 end
			   val (is, bindings', xs, xs') = traverse (bindings, is, env)
			   val (is, body') = walk (body, is, Env.extendlis (xs, xs', env))
		       in (is, LET (bindings', body'))
		       end
		   | walk (Syntax0.LETSTAR (nil, body), is, env)
		     = walk (body, is, env)
		   | walk (Syntax0.LETSTAR (bindings, body), is, env)
		     = let fun traverse (nil, is, env)
			       = (is, nil, nil, nil)
			     | traverse ((x, e) :: bindings, is, env)
			       = let val x' = Gensym.new (x ^ "%")
				     val (is, e') = walk (e, is, env)
				     val (is, bindings', xs, xs') = traverse (bindings, is, Env.extend (x, IDENTIFIER x', env))
				 in (is, (x', e') :: bindings', x :: xs, (IDENTIFIER x') :: xs')
				 end
			   val (is, bindings', xs, xs') = traverse (bindings, is, env)
			   val (is, body') = walk (body, is, Env.extendlis (List.rev xs, List.rev xs', env))
		       in (is, LETSTAR (bindings', body'))
		       end
		   | walk (Syntax0.LETREC (nil, body), is, env)
		     = walk (body, is, env)
		   | walk (Syntax0.LETREC (bindings, body), is, env)
		     = let val fs = List_misc.map (fn (x, _, _) => x, bindings)
			   val fs' = List_misc.map (fn f => Gensym.new (f ^ "%"), fs)
			   val _ = Mutable_variables.insert_letrec_mutables fs'
			   fun traverse (nil, nil, is, env)
			       = (is, nil)
			     | traverse ((_, Syntax0.VARIADIC x, e) :: bindings, f' :: fs', is, env)
			       = let val x' = Gensym.new (x ^ "%")
				     val (is, e') = walk (e, is, Env.extend (x, IDENTIFIER x', env))
				     val (is, bindings') = traverse (bindings, fs', is, env)
				 in (is, (f', VARIADIC x', e') :: bindings')
				 end
			     | traverse ((_, Syntax0.FIXED xs, e) :: bindings, f' :: fs', is, env)
			       = let val xs' = List_misc.map (fn x => Gensym.new (x ^ "%"), xs)
				     val (is, e') = walk (e, is, Env.extendlis (xs, List_misc.map (IDENTIFIER, xs'), env))
				     val (is, bindings') = traverse (bindings, fs', is, env)
				 in (is, (f', FIXED xs', e') :: bindings')
				 end
			     | traverse _
			       = raise MUMBLE_LETREC
			   val env' = Env.extendlis (fs, List_misc.map (IDENTIFIER, fs'), env)
			   val (is, bindings') = traverse (bindings, fs', is, env')
			   val (is, body') = walk (body, is, env')
		       in (is, LETREC (bindings', body'))
		       end
		   | walk (Syntax0.BEGIN (e, es), is, env)
		     = let val (e, es) = flatten_begin (e, es)
			   val (is, e') = walk (e, is, env)
			   fun traverse (nil, is)
			       = (is, nil)
			     | traverse (e :: es, is)
			       = let val (is, e') = walk (e, is, env)
				     val (is, es') = traverse (es, is)
				 in (is, e' :: es')
				 end
			   val (is, es') = traverse (es, is)
		       in (is, BEGIN (e', es'))
		       end
		   | walk (Syntax0.SUSPEND e, is, env)
		     = let val (is, e') = walk (e, is, env)
		       in (is, LAMBDA (FIXED nil, e'))
		       end
		   | walk (Syntax0.DELAY e, is, env)
		     = let val (is, e') = walk (e, is, env)
			   val reference = Gensym.new "delay%"
			   val tmp = Gensym.new "tmp%"
		       in (is, LET ([(reference,
				      APPLICATION (IDENTIFIER "vector",
						   [BOOLEAN false,
						    STRING "not computed yet"]))],
				    LAMBDA (FIXED nil,
					    IF (APPLICATION (IDENTIFIER "vector-ref",
							     [IDENTIFIER reference,
							      INTEGER 0]),
						APPLICATION (IDENTIFIER "vector-ref",
							     [IDENTIFIER reference,
							      INTEGER 1]),
						BEGIN (APPLICATION (IDENTIFIER "vector-set!",
								    [IDENTIFIER reference,
								     INTEGER 0,
								     BOOLEAN true]),
						       [LET ([(tmp, e')],
							     BEGIN (APPLICATION (IDENTIFIER "vector-set!",
										 [IDENTIFIER reference,
										  INTEGER 1,
										  IDENTIFIER tmp]),
								    [IDENTIFIER tmp]))])))))
		       end
		   | walk (Syntax0.SETBANG (x, e), is, env)
		     = let val (is, e') = walk (e, is, env)
		       in (is, SETBANG (case Env.lookup (x, env)
					  of (SOME (IDENTIFIER y))
					     => (if Mutable_variables.occurp_letrec_mutable y
						 then raise THOU_SHALL_NOT_SETBANG_A_LETREC_DECLARED_VARIABLE
						 else ();
						 Mutable_variables.insert_local_mutable y;
						 y)
					   | (SOME _)
					     => raise MUMBLE_ENVIRONMENT
					   | NONE
					     => if (Mutable_variables.globalp x) orelse
						   (Mutable_variables.predefinedp x)
						then (Mutable_variables.insert_toplevel_mutable x; x)
						else (TextIO.output (TextIO.stdOut, "Undeclared variable: ");
						      TextIO.output (TextIO.stdOut, x);
						      raise UNDECLARED_VARIABLE),
					e'))
		       end
		   | walk (Syntax0.APPLICATION (e, es), is, env)
		     = let val (is, e') = walk (e, is, env)
			   fun traverse (nil, is)
			       = (is, nil)
			     | traverse (e :: es, is)
			       = let val (is, e') = walk (e, is, env)
				     val (is, es') = traverse (es, is)
				 in (is, e' :: es')
				 end
			   val (is, es') = traverse (es, is)
		       in (is, APPLICATION (e', es'))
		       end
	     in walk (e, nil, Env.empty)
	     end

       fun desugar_definition (Syntax0.DEFINE_RECORD (name, fields))
	   = let val len = (List.length fields) + 1
		 val make_name = "make-" ^ name
		 val namep = "is-" ^ name ^ "?"
		 val indices = let fun loop i
		                       = if i = len
					 then nil
					 else i :: (loop (i + 1))
			       in loop 1
			       end
		 val fresh_fields = List_misc.map (fn field => Gensym.new (field ^ "_"),
						   fields)
		 val v = Gensym.new "record%"
	     in [DEFINE (make_name,
			 LAMBDA (FIXED fresh_fields,
				 APPLICATION (IDENTIFIER "vector",
					      (SYMBOL name)
					      :: (List_misc.map (IDENTIFIER,
								 fresh_fields))))),
		 DEFINE (namep,
			 LAMBDA (FIXED [v],
				 IF (APPLICATION (IDENTIFIER "vector?",
						  [IDENTIFIER v]),
				     IF (APPLICATION (IDENTIFIER "=",
						      [APPLICATION (IDENTIFIER "vector-length",
								    [IDENTIFIER v]),
						       INTEGER len]),
					 APPLICATION (IDENTIFIER "eqv?",
						      [APPLICATION (IDENTIFIER "vector-ref",
								    [IDENTIFIER v,
								     INTEGER 0]),
						       SYMBOL name]),
					 BOOLEAN false),
				     BOOLEAN false)))]
	     end
	 | desugar_definition (Syntax0.DEFINE (x, e))
	   = let val (is, e') = desugar_expression e
	     in is @ [DEFINE (x, e')]
	     end
    end

    fun collect_global_names nil
	= nil
      | collect_global_names ((Syntax0.DEFINE_RECORD (x, _)) :: ds)
	= ("make-" ^ x) :: ("is-" ^ x ^ "?") :: (collect_global_names ds)
      | collect_global_names ((Syntax0.DEFINE (x, _)) :: ds)
	= x :: (collect_global_names ds)

    fun main (Syntax0.PROGRAM (ds, e))
	= let val () = Gensym.init ()
	      val () = Mutable_variables.init (collect_global_names ds)
	      val ds = List_misc.map_append (desugar_definition, ds)
	  in case desugar_expression e
	       of (nil, e')
		  => Syntax1.PROGRAM (ds, e')
		| (is, e')
		  => Syntax1.PROGRAM (ds @ is, e')
	  end
  end;

(* ********** *)

(* end of DAIMI-Scheme.front-end/1desugarer.sml *)
