(* DAIMI-Scheme.front-end/0parser.sml *)
(* a raw parser for DAIMI Scheme      *)
(* Olivier Danvy <danvy@brics.dk>     *)
(* October 2003                       *)

(* ********** *)

structure Identifier
= struct
    type identifier = string

    type toplevel_identifier = identifier
  end;

(* ********** *)

structure Syntax0
= struct
    type identifier = string

    datatype formals = VARIADIC of Identifier.identifier
                     | FIXED of Identifier.identifier list

    datatype simple_datum = SIMPLE_INTEGER of int
                          | SIMPLE_BOOLEAN of bool
                          | SIMPLE_CHARACTER of char
                          | SIMPLE_SYMBOL of string

    datatype expression = INTEGER of int
                        | BOOLEAN of bool
                        | CHARACTER of char
                        | STRING of string
			| IDENTIFIER of Identifier.identifier
                        | QUOTATION of Sexpr.sexpr
                        | LAMBDA of formals * expression
                        | IF of expression * expression * expression
                        | COND of (expression * expression) list * expression
                        | CASE of expression * (simple_datum list * expression) list * expression
                        | CASE_RECORD of expression * ((Identifier.identifier * Identifier.identifier list) * expression) list * expression
                        | AND of expression list
                        | OR of expression list
                        | LET of (Identifier.identifier * expression) list * expression
                        | LETSTAR of (Identifier.identifier * expression) list * expression
                        | LETREC of (Identifier.identifier * formals * expression) list * expression
                        | BEGIN of expression * expression list
                        | SUSPEND of expression
                        | DELAY of expression
                        | SETBANG of Identifier.identifier * expression
			| APPLICATION of expression * expression list

    datatype definition = DEFINE of Identifier.toplevel_identifier * expression
                        | DEFINE_RECORD of Identifier.identifier * Identifier.identifier list

    datatype program = PROGRAM of definition list * expression
  end;

(* ********** *)

(* exception UNIMPLEMENTED; *)

(* ********** *)

structure List_utilities
= struct
    fun occurp (x, xs)
	= let fun walk nil
		  = false
		| walk (x' :: xs)
		  = (x = x') orelse (walk xs)
	  in walk xs
	  end

    fun all_differentp nil
	= true
      | all_differentp (x :: xs)
        = (not (occurp (x, xs))) andalso (all_differentp xs)
  end;

(* ********** *)

signature SYNTAX0_PARSER
= sig
    val parse_expression : Sexpr.sexpr -> Syntax0.expression
    val main : string -> Syntax0.program
  end;

structure Syntax0_parser : SYNTAX0_PARSER
= struct
    local open Sexpr
    in
       fun sexpr_to_list e
	   = let exception NOT_GOOD
		 fun walk NIL
		     = nil
		   | walk (PAIR (e, es))
		     = e :: (walk es)
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (walk e)) handle NOT_GOOD => NONE
	     end

       fun sexpr_to_list_of_names e
	   = let exception NOT_GOOD
		 fun walk NIL
		     = nil
		   | walk (PAIR (SYMBOL name, es))
		     = name :: (walk es)
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (walk e)) handle NOT_GOOD => NONE
	     end
    end

    fun variable_ok x
	= not (List_utilities.occurp (x,
				      ["define",
				       "lambda",
				       "if",
				       "set!",
				       "let",
				       "let*",
				       "letrec",
				       "cond",
				       "case",
				       "else",
				       "and",
				       "or",
				       "begin",
				       "delay",
				       "suspend",
				       "define-record",
				       "case-record",
				       "quote",
				       "load",
				       "load-relative"]))

    fun variables_ok nil
	= true
      | variables_ok (x :: xs)
	= (variable_ok x) andalso (variables_ok xs)

    local open Sexpr
    in
       fun quote_ok (PAIR (e, NIL))
	   = SOME e
	 | quote_ok _
	   = NONE

       fun lambda_ok (PAIR (SYMBOL x, PAIR (body, NIL)))
	   = if variable_ok x
	     then SOME (Syntax0.VARIADIC x, body)
	     else NONE
	 | lambda_ok (PAIR (formals, PAIR (body, NIL)))
	   = (case sexpr_to_list_of_names formals
		of (SOME xs)
		   => if (List_utilities.all_differentp xs)
		         andalso
			 (variables_ok xs)
		      then SOME (Syntax0.FIXED xs, body)
		      else NONE
		 | NONE
		   => NONE)
	 | lambda_ok _
	   = NONE

       fun if_ok (Sexpr.PAIR (test,
			      Sexpr.PAIR (consequent,
					  Sexpr.PAIR (alternative,
						      Sexpr.NIL))))
	   = SOME (test, consequent, alternative)
	 | if_ok _
	   = NONE

       fun cond_ok e
	   = let exception NOT_GOOD
		 fun walk (PAIR (PAIR (SYMBOL "else", PAIR (e, NIL)),
				 NIL))
		     = (nil, e)
		   | walk (PAIR (PAIR (test, PAIR (consequent, NIL)),
				 rest))
		     = let val (clauses, alternative) = walk rest
		       in ((test, consequent) :: clauses, alternative)
		       end
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (walk e)) handle NOT_GOOD => NONE
	     end

       fun case_ok (PAIR (test, rest))
	   = let exception NOT_GOOD
		 fun simple_datum_ok (INTEGER n)
		     = Syntax0.SIMPLE_INTEGER n
		   | simple_datum_ok (BOOLEAN n)
		     = Syntax0.SIMPLE_BOOLEAN n
		   | simple_datum_ok (CHARACTER n)
		     = Syntax0.SIMPLE_CHARACTER n
		   | simple_datum_ok (SYMBOL n)
		     = Syntax0.SIMPLE_SYMBOL n
		   | simple_datum_ok _
		     = raise NOT_GOOD
		 fun simple_data_ok NIL
		     = nil
		   | simple_data_ok (PAIR (d, rest))
		     = (simple_datum_ok d) :: (simple_data_ok rest)
		   | simple_data_ok _
		     = raise NOT_GOOD
		 fun walk (PAIR (PAIR (SYMBOL "else", PAIR (alternative, NIL)),
				 NIL))
		     = (nil, alternative)
		   | walk (PAIR (PAIR (e, PAIR (consequent, NIL)),
				 rest))
		     = let val ds = simple_data_ok e
			   val (clauses, alternative) = walk rest
		       in ((ds, consequent) :: clauses, alternative)
		       end
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (test, walk rest)) handle NOT_GOOD => NONE
	     end
	 | case_ok _
	   = NONE

       fun case_record_ok (PAIR (test, rest))
	   = let exception NOT_GOOD
		 fun all_symbols NIL
		     = nil
		   | all_symbols (PAIR (SYMBOL x, e))
		     = x :: (all_symbols e)
		   | all_symbols _
		     = raise NOT_GOOD
		 fun walk (PAIR (PAIR (SYMBOL "else", PAIR (alternative, NIL)),
				 NIL))
		     = (nil, alternative)
		   | walk (PAIR (PAIR (PAIR (SYMBOL x, e), PAIR (consequent, NIL)),
				 rest))
		     = let val xs = all_symbols e
			   val (clauses, alternative) = walk rest
		       in if (variable_ok x)
			     andalso
			     (variables_ok xs)
			     andalso
			     (not (List_utilities.occurp (x, xs)))
			     andalso
			     (List_utilities.all_differentp xs)
			  then (((x, xs), consequent) :: clauses, alternative)
			  else raise NOT_GOOD
		       end
		   | walk _
		     = raise NOT_GOOD
	     in  (SOME (test, walk rest)) handle NOT_GOOD => NONE
	     end
	 | case_record_ok _
	   = NONE

       fun list_ok e
	   = let exception NOT_GOOD
		 fun walk NIL
		     = nil
		   | walk (PAIR (e, rest))
		     = e :: (walk rest)
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (walk e)) handle NOT_GOOD => NONE
	     end

       fun let_ok (PAIR (e, PAIR (body, NIL)))
	   = let exception NOT_GOOD
		 fun walk NIL
		     = (nil, nil)
		   | walk (PAIR (PAIR (SYMBOL x, PAIR (e, NIL)), rest))
		     = let val (xs, clauses) = walk rest
		       in if (variable_ok x)
			     andalso
			     (not (List_utilities.occurp (x, xs)))
			  then (x :: xs, (x, e) :: clauses)
			  else raise NOT_GOOD
		       end
		   | walk _
		     = raise NOT_GOOD
		 val (_, clauses) = walk e
	     in (SOME (clauses, body)) handle NOT_GOOD => NONE
	     end
	 | let_ok _
	   = NONE

       fun letstar_ok (PAIR (e, PAIR (body, NIL)))
	   = let exception NOT_GOOD
		 fun walk NIL
		     = nil
		   | walk (PAIR (PAIR (SYMBOL x, PAIR (e, NIL)), rest))
		     = if variable_ok x
		       then (x, e) :: (walk rest)
		       else raise NOT_GOOD
		   | walk _
		     = raise NOT_GOOD
	     in (SOME (walk e, body)) handle NOT_GOOD => NONE
	     end
	 | letstar_ok _
	   = NONE

       fun letrec_ok (PAIR (e, PAIR (body, NIL)))
	   = let exception NOT_GOOD
		 fun walk NIL
		     = (nil, nil)
		   | walk (PAIR (PAIR (SYMBOL x, PAIR (PAIR (SYMBOL "lambda", rest'), NIL)), rest))
		     = if variable_ok x
		       then (case lambda_ok rest'
			  of (SOME (formals, body'))
			     => let val (xs, clauses) = (walk rest)
				in if List_utilities.occurp (x, xs)
				   then raise NOT_GOOD
				   else (x :: xs, (x, formals, body') :: clauses)
				end
			   | NONE
			     => raise NOT_GOOD)
		       else raise NOT_GOOD
		   | walk _
		     = raise NOT_GOOD
		 val (_, clauses) = walk e
	     in (SOME (clauses, body)) handle NOT_GOOD => NONE
	     end
	 | letrec_ok _
	   = NONE

       fun begin_ok (PAIR (e, rest))
	   = (case list_ok rest
		of (SOME es)
		   => SOME (e, es)
		 | _
		   => NONE)
	 | begin_ok _
	   = NONE

       fun one_ok (PAIR (e, NIL))
	   = SOME e
	 | one_ok _
	   = NONE

       fun setbang_ok (PAIR (SYMBOL x, PAIR (e, NIL)))
	   = if variable_ok x
	     then SOME (x, e)
	     else NONE
	 | setbang_ok _
	   = NONE
    end

    exception IMPROPER_EXPRESSION of Sexpr.sexpr

    fun parse_expression (Sexpr.INTEGER n)
	= Syntax0.INTEGER n
      | parse_expression (Sexpr.BOOLEAN b)
	= Syntax0.BOOLEAN b
      | parse_expression (Sexpr.CHARACTER c)
	= Syntax0.CHARACTER c
      | parse_expression (Sexpr.STRING s)
	= Syntax0.STRING s
      | parse_expression (Sexpr.SYMBOL s)
	= if variable_ok s
	  then Syntax0.IDENTIFIER s
	  else raise (IMPROPER_EXPRESSION (Sexpr.SYMBOL s)) 
      | parse_expression (Sexpr.PAIR (e, rest))
	= parse_expression2 (e, rest)
      | parse_expression e
	= raise (IMPROPER_EXPRESSION e)
    and parse_expression2 (Sexpr.SYMBOL "quote", rest)
	= (case quote_ok rest
	     of (SOME e)
		=> Syntax0.QUOTATION e
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "quote", rest))))
      | parse_expression2 (Sexpr.SYMBOL "lambda", rest)
	= (case lambda_ok rest
	     of (SOME (formals, e))
		=> Syntax0.LAMBDA (formals, parse_expression e)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "lambda", rest))))
      | parse_expression2 (Sexpr.SYMBOL "if", rest)
	= (case if_ok rest
	     of (SOME (test, consequent, alternative))
		=> Syntax0.IF (parse_expression test,
			       parse_expression consequent,
			       parse_expression alternative)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "if", rest))))
      | parse_expression2 (Sexpr.SYMBOL "cond", rest)
        = (case cond_ok rest
	     of (SOME (clauses, alternative))
		=> Syntax0.COND (map (fn (test, consequent)
					 => (parse_expression test,
					     parse_expression consequent))
				     clauses,
				 parse_expression alternative)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "cond", rest))))
      | parse_expression2 (Sexpr.SYMBOL "case", rest)
        = (case case_ok rest
	     of (SOME (test, (clauses, alternative)))
		=> Syntax0.CASE (parse_expression test,
				 map (fn (simple_data, consequent)
					 => (simple_data, parse_expression consequent))
				     clauses,
				 parse_expression alternative)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "case", rest))))
      | parse_expression2 (Sexpr.SYMBOL "case-record", rest)
	= (case case_record_ok rest
	     of (SOME (test, (clauses, alternative)))
		=> Syntax0.CASE_RECORD (parse_expression test,
					map (fn (formals, consequent)
						=> (formals, parse_expression consequent))
					    clauses,
				        parse_expression alternative)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "case-record", rest))))
      | parse_expression2 (Sexpr.SYMBOL "and", rest)
	= (case list_ok rest
	     of (SOME es)
		=> Syntax0.AND (map parse_expression es)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "and", rest))))
      | parse_expression2 (Sexpr.SYMBOL "or", rest)
	= (case list_ok rest
	     of (SOME es)
		=> Syntax0.OR (map parse_expression es)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "or", rest))))
      | parse_expression2 (Sexpr.SYMBOL "let", rest)
	= (case let_ok rest
	     of (SOME (clauses, body))
		=> Syntax0.LET (map (fn (x, e)
					=> (x, parse_expression e))
				    clauses,
				parse_expression body)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "let", rest))))
      | parse_expression2 (Sexpr.SYMBOL "let*", rest)
	= (case letstar_ok rest
	     of (SOME (clauses, body))
		=> Syntax0.LETSTAR (map (fn (x, e)
					    => (x, parse_expression e))
				        clauses,
				parse_expression body)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "let*", rest))))
      | parse_expression2 (Sexpr.SYMBOL "letrec", rest)
	= (case letrec_ok rest
	     of (SOME (clauses, body))
		=> Syntax0.LETREC (map (fn (x, formals, e)
					    => (x, formals, parse_expression e))
				        clauses,
				parse_expression body)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "letrec", rest))))
      | parse_expression2 (Sexpr.SYMBOL "begin", rest)
	= (case begin_ok rest
	     of (SOME (e, es))
		=> Syntax0.BEGIN (parse_expression e, map parse_expression es)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "begin", rest))))
      | parse_expression2 (Sexpr.SYMBOL "suspend", rest)
	= (case one_ok rest
	     of (SOME e)
		=> Syntax0.SUSPEND (parse_expression e)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "suspend", rest))))
      | parse_expression2 (Sexpr.SYMBOL "delay", rest)
	= (case one_ok rest
	     of (SOME e)
		=> Syntax0.DELAY (parse_expression e)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "delay", rest))))
      | parse_expression2 (Sexpr.SYMBOL "set!", rest)
	= (case setbang_ok rest
	     of (SOME (x, e))
		=> Syntax0.SETBANG (x, parse_expression e)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (Sexpr.SYMBOL "set!", rest))))
      | parse_expression2 (e, rest)
	= (case list_ok rest
	     of (SOME es)
		=> Syntax0.APPLICATION (parse_expression e, map parse_expression es)
	      | NONE
		=> raise (IMPROPER_EXPRESSION (Sexpr.PAIR (e, rest))))

    exception IMPROPER_RECORD_DEFINITION of Sexpr.sexpr

    fun parse_record_definition (e as Sexpr.PAIR (Sexpr.PAIR (Sexpr.SYMBOL name, rest), Sexpr.NIL))
	= (case sexpr_to_list_of_names rest
	     of NONE
		=> raise (IMPROPER_RECORD_DEFINITION e)
	      | (SOME names)
		=> if List_utilities.all_differentp (name :: names)
		   then Syntax0.DEFINE_RECORD (name, names)
		   else raise (IMPROPER_RECORD_DEFINITION e))
      | parse_record_definition e
	= raise (IMPROPER_RECORD_DEFINITION e)

    exception IMPROPER_DEFINITION of Sexpr.sexpr

    fun parse_definition (Sexpr.PAIR (Sexpr.SYMBOL name, Sexpr.PAIR (named_sexpr, Sexpr.NIL)))
	= Syntax0.DEFINE (name, parse_expression named_sexpr)
      | parse_definition e
	= raise (IMPROPER_DEFINITION e)

    datatype filename = CURRENT of string
		      | ABSOLUTE of string * string
		      | RELATIVE of string * string

    fun split_filename filename
	= let val length = String.size filename
	      fun walk ~1
		  = NONE
		| walk offset
		  = if String.sub (filename, offset) = #"/"
		    then SOME (offset + 1)
		    else walk (offset - 1)
	  in case walk (length - 1)
	       of NONE
		  => CURRENT filename
		| (SOME offset)
		  => (if String.sub (filename, 0) = #"/"
		      then ABSOLUTE 
		      else RELATIVE)
		     (String.substring (filename, 0, offset),
		      String.substring (filename, offset, length - offset))
	  end

    fun normalize_filename (filename, directory)
	= (case split_filename filename
	     of (CURRENT _)
		=> (directory, directory ^ filename)
	      | (ABSOLUTE (new_directory, _))
		=> (new_directory, filename)
	      | (RELATIVE (new_directory, _))
		=> (directory ^ new_directory, directory ^ filename))

    exception CIRCULAR_LOAD of string * string list
    exception CIRCULAR_LOAD_RELATIVE of string * string list
    exception NOT_A_DEFINITION of Sexpr.sexpr
    exception WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY

    fun main filename
	= let fun traverse (sexpr :: nil, filenames, current_directory)
		  = (nil, parse_expression sexpr)
		| traverse ((Sexpr.PAIR (Sexpr.SYMBOL "define-record", rest)) :: sexprs,
			    filenames,
			    current_directory)
		  = let val d = parse_record_definition rest
			val (ds, e) = traverse (sexprs, filenames, current_directory)
		    in (d :: ds, e)
		    end
		| traverse ((Sexpr.PAIR (Sexpr.SYMBOL "define", rest)) :: sexprs,
			    filenames,
			    current_directory)
		  = let val d = parse_definition rest
		        val (ds, e) = traverse (sexprs, filenames, current_directory)
		    in (d :: ds, e)
		    end
		| traverse ((Sexpr.PAIR (Sexpr.SYMBOL "load",
					 Sexpr.PAIR (Sexpr.STRING filename,
						     Sexpr.NIL))) :: sexprs,
			    filenames,
			    current_directory)
		  = if List_utilities.occurp (filename, filenames)
		    then raise (CIRCULAR_LOAD (filename, filenames))
		    else let val ds' = traverse_file (Sexpr_parser.read_file filename,
						      filename :: filenames,
						      "")
			     val (ds, e) = traverse (sexprs, filenames, current_directory)
			 in (ds' @ ds, e)
			 end
		| traverse ((Sexpr.PAIR (Sexpr.SYMBOL "load-relative",
					 Sexpr.PAIR (Sexpr.STRING filename,
						     Sexpr.NIL))) :: sexprs,
			    filenames,
			    current_directory)
		  = let val (new_directory, complete_filename)
			    = normalize_filename (filename, current_directory)
		    in if List_utilities.occurp (complete_filename, filenames)
		       then raise (CIRCULAR_LOAD_RELATIVE (complete_filename, filenames))
		       else let val ds' = traverse_file (Sexpr_parser.read_file complete_filename,
							 complete_filename :: filenames,
							 new_directory)
				val (ds, e) = traverse (sexprs, filenames, current_directory)
			    in (ds' @ ds, e)
			    end
		    end
		| traverse (whatever :: sexprs, filenames, current_directory)
		  = raise (NOT_A_DEFINITION whatever)
		| traverse (nil, filenames, current_directory)
		  = raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY
	      and traverse_file (nil, filenames, current_directory)
		  = nil
		| traverse_file ((Sexpr.PAIR (Sexpr.SYMBOL "define-record", rest)) :: sexprs,
				 filenames,
				 current_directory)
		  = (parse_record_definition rest)
		    :: (traverse_file (sexprs, filenames, current_directory))
		| traverse_file ((Sexpr.PAIR (Sexpr.SYMBOL "define", rest)) :: sexprs,
				 filenames,
				 current_directory)
		  = (parse_definition rest)
                    :: (traverse_file (sexprs, filenames, current_directory))
		| traverse_file ((Sexpr.PAIR (Sexpr.SYMBOL "load",
					 Sexpr.PAIR (Sexpr.STRING filename,
						     Sexpr.NIL))) :: sexprs,
			    filenames,
				 current_directory)
		  = if List_utilities.occurp (filename, filenames)
		    then raise (CIRCULAR_LOAD (filename, filenames))
		    else (traverse_file (Sexpr_parser.read_file filename,
					 filename :: filenames,
					 ""))
			 @ (traverse_file (sexprs, filenames, current_directory))
		| traverse_file ((Sexpr.PAIR (Sexpr.SYMBOL "load-relative",
					 Sexpr.PAIR (Sexpr.STRING filename,
						     Sexpr.NIL))) :: sexprs,
				 filenames,
				 current_directory)
		  = let val (new_directory, complete_filename)
			    = normalize_filename (filename, current_directory)
		    in if List_utilities.occurp (complete_filename, filenames)
		       then raise (CIRCULAR_LOAD_RELATIVE (complete_filename, filenames))
		       else (traverse_file (Sexpr_parser.read_file complete_filename,
					    complete_filename :: filenames,
					    new_directory))
			    @ (traverse_file (sexprs, filenames, current_directory))
		    end
		| traverse_file (whatever :: sexprs, filenames, current_directory)
		  = raise (NOT_A_DEFINITION whatever)
	      val (current_directory, absolute_filename)
		  = normalize_filename (filename,
					(* (OS.FileSys.getDir ()) ^ "/" *)
					"")
	  in (Syntax0.PROGRAM (traverse (Sexpr_parser.read_file filename,
					 [absolute_filename],
					 current_directory)))
	     handle (IMPROPER_EXPRESSION e)
		    => (TextIO.output (TextIO.stdOut, "Improper expression:\n");
			Sexpr.print e;
			raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY)
		  | (NOT_A_DEFINITION e)
		    => (TextIO.output (TextIO.stdOut, "Not a definition:\n");
			Sexpr.print e;
			raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY)
		  | (IMPROPER_DEFINITION e)
		    => (TextIO.output (TextIO.stdOut, "Improper definition:\n");
			Sexpr.print e;
			raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY)
		  | (IMPROPER_RECORD_DEFINITION e)
		    => (TextIO.output (TextIO.stdOut, "Improper record definition:\n");
			Sexpr.print e;
			raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY)
		  | (CIRCULAR_LOAD_RELATIVE (filename, filenames))
		    => (TextIO.output (TextIO.stdOut, "Circular load-relative: ");
			TextIO.output (TextIO.stdOut, filename);
			TextIO.output (TextIO.stdOut, " in [");
			let fun walk (filename, nil)
			        = (TextIO.output (TextIO.stdOut, "\"");
				   TextIO.output (TextIO.stdOut, filename);
				   TextIO.output (TextIO.stdOut, "\""))
			      | walk (filename, filename' :: filenames)
			        = (TextIO.output (TextIO.stdOut, "\"");
				   TextIO.output (TextIO.stdOut, filename);
				   TextIO.output (TextIO.stdOut, "\", ");
				   walk (filename', filenames))
			in case filenames
			     of nil
				=> ()
			      | (filename :: filenames)
				=> walk (filename, filenames)
			end;
			TextIO.output (TextIO.stdOut, "]\n");
			raise WHAT_ELSE_DO_YOU_WANT_TO_COMPILE_TODAY)
	  end
  end;

(* ********** *)

(* end of DAIMI-Scheme.front-end/0parser.sml *)
