(* DAIMI-Scheme.front-end/front-end.sml *)
(* a front-end for DAIMI Scheme         *)
(* Olivier Danvy <danvy@brics.dk>       *)
(* October 2003                         *)

(* ********** *)

signature FRONT_END
= sig
    val main : string -> unit
    val test : string -> unit
  end;

(* ********** *) 

structure Front_end : FRONT_END
= struct
    local open Syntax2
    in 
       fun write_program (Syntax2.PROGRAM (ds, e), out)
	   = let fun write_e (INTEGER n)
		     = if n >= 0
		       then TextIO.output (out, Int.toString n)
		       else (TextIO.output (out, "-");
			     TextIO.output (out, Int.toString (~n)))
		   | write_e (BOOLEAN true)
		     = TextIO.output (out, "#t")
		   | write_e (BOOLEAN false)
		     = TextIO.output (out, "#f")
		   | write_e (CHARACTER #"\t")
		     = TextIO.output (out, "#\\tab")
		   | write_e (CHARACTER #"\n")
		     = TextIO.output (out, "#\\newline")
		   | write_e (CHARACTER #"\\")
		     = TextIO.output (out, "#\\\\")
		   | write_e (CHARACTER #"\"")
		     = TextIO.output (out, "#\\\"")
		   | write_e (CHARACTER c)
		     = (TextIO.output (out, "#\\");
			TextIO.output (out, Char.toString c))
		   | write_e (STRING s)
		     = (TextIO.output (out, "\"");
			TextIO.output (out, s);
			TextIO.output (out, "\""))
		   | write_e (SYMBOL s)
		     = (TextIO.output (out, "'");
			TextIO.output (out, s))
		   | write_e NIL
		     = TextIO.output (out, "'()")
		   | write_e (IDENTIFIER x)
		     = TextIO.output (out, x)
		   | write_e (LAMBDA (VARIADIC xs, e))
		     = (TextIO.output (out, "(lambda ");
		        TextIO.output (out, xs);
			TextIO.output (out, " ");
			write_e e;
			TextIO.output (out, ")"))
		   | write_e (LAMBDA (FIXED xs, e))
		     = (TextIO.output (out, "(lambda (");
		        write_xs xs;
			TextIO.output (out, ") ");
			write_e e;
			TextIO.output (out, ")"))
		   | write_e (IF (test, consequent, alternative))
		     = (TextIO.output (out, "(if ");
			write_e test;
			TextIO.output (out, " ");
			write_e consequent;
			TextIO.output (out, " ");
			write_e alternative;
			TextIO.output (out, ")"))
		   | write_e (LET (clauses, body))
		     = (TextIO.output (out, "(let (");
			write_cs clauses;
			TextIO.output (out, ") ");
			write_e body;
		        TextIO.output (out, ")"))
		   | write_e (LETSTAR (clauses, body))
		     = (TextIO.output (out, "(let* (");
			write_cs clauses;
			TextIO.output (out, ") ");
			write_e body;
		        TextIO.output (out, ")"))
		   | write_e (LETREC (clauses, body))
		     = (TextIO.output (out, "(letrec (");
			write_ls clauses;
			TextIO.output (out, ") ");
			write_e body;
		        TextIO.output (out, ")"))
		   | write_e (BEGIN (e, es))
		     = (TextIO.output (out, "(begin ");
			write_es (e, es);
		        TextIO.output (out, ")"))
		   | write_e (SETBANG (x, e))
		     = (TextIO.output (out, "(set! ");
			TextIO.output (out, x);
			TextIO.output (out, " ");
			write_e e;
		        TextIO.output (out, ")"))
		   | write_e (APPLICATION (e, es))
		     = (TextIO.output (out, "(");
			write_es (e, es);
		        TextIO.output (out, ")"))
		 and write_xs nil
		     = ()
		   | write_xs (x :: nil)
		     = TextIO.output (out, x)
		   | write_xs (x :: xs)
		     = let fun walk (x, nil)
			       = TextIO.output (out, x)
			     | walk (x, x' :: xs)
			       = (TextIO.output (out, x);
				  TextIO.output (out, " ");
				  walk (x', xs))
		       in walk (x, xs)
		       end
		 and write_cs nil
		     = ()
		   | write_cs ((x, e) :: nil)
		     = (TextIO.output (out, "[");
			TextIO.output (out, x);
			TextIO.output (out, " ");
			write_e e;
			TextIO.output (out, "]"))
		   | write_cs (c :: cs)
		     = let fun walk ((x, e), nil)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, x);
				  TextIO.output (out, " ");
				  write_e e;
				  TextIO.output (out, "]"))
			     | walk ((x, e), c :: cs)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, x);
				  TextIO.output (out, " ");
				  write_e e;
				  TextIO.output (out, "] ");
				  walk (c, cs))
		       in walk (c, cs)
		       end
		 and write_ls nil
		     = ()
		   | write_ls ((f, VARIADIC x, e) :: nil)
		     = (TextIO.output (out, "[");
			TextIO.output (out, f);
			TextIO.output (out, " ");
			TextIO.output (out, "(lambda ");
			TextIO.output (out, x);
			TextIO.output (out, " ");
			write_e e;
			TextIO.output (out, ")]"))
		   | write_ls ((f, FIXED xs, e) :: nil)
		     = (TextIO.output (out, "[");
			TextIO.output (out, f);
			TextIO.output (out, " ");
			TextIO.output (out, "(lambda (");
			write_xs xs;
			TextIO.output (out, ") ");
			write_e e;
			TextIO.output (out, ")]"))
		   | write_ls (c :: cs)
		     = let fun walk ((f, VARIADIC x, e), nil)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, f);
				  TextIO.output (out, " ");
				  TextIO.output (out, "(lambda ");
				  TextIO.output (out, x);
				  TextIO.output (out, " ");
				  write_e e;
				  TextIO.output (out, ")]"))
			     | walk ((f, FIXED xs, e), nil)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, f);
				  TextIO.output (out, " ");
				  TextIO.output (out, "(lambda (");
				  write_xs xs;
				  TextIO.output (out, ") ");
				  write_e e;
				  TextIO.output (out, ")]"))
			     | walk ((f, VARIADIC x, e), c :: cs)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, f);
				  TextIO.output (out, " ");
				  TextIO.output (out, "(lambda ");
				  TextIO.output (out, x);
				  TextIO.output (out, " ");
				  write_e e;
				  TextIO.output (out, ")] ");
				  walk (c, cs))
			     | walk ((f, FIXED xs, e), c :: cs)
			       = (TextIO.output (out, "[");
				  TextIO.output (out, f);
				  TextIO.output (out, " ");
				  TextIO.output (out, "(lambda (");
				  write_xs xs;
				  TextIO.output (out, ") ");
				  write_e e;
				  TextIO.output (out, ")] ");
				  walk (c, cs))
		       in walk (c, cs)
		       end
		 and write_es (e, nil)
		     = write_e e
		   | write_es (e, e' :: es)
		     = (write_e e;
			TextIO.output (out, " ");
			write_es (e', es))
		 and write_ds nil
		     = ()
		   | write_ds ((DEFINE (x, e)) :: ds)
		     = (TextIO.output (out, "(define ");
			TextIO.output (out, x);
			TextIO.output (out, " ");
			write_e e;
			TextIO.output (out, ")\n");
			write_ds ds)
	     in (write_ds ds;
		 write_e e;
		 TextIO.output (out, "\n"))
	     end

       fun manufacture_target_name filename
	   = let val length = String.size filename
	     in if (length > 4)
		   andalso
		   (String.sub (filename, length - 1) = #"m")
		   andalso
		   (String.sub (filename, length - 2) = #"c")
		   andalso
		   (String.sub (filename, length - 3) = #"s")
		   andalso
		   (String.sub (filename, length - 4) = #".")
		then (String.substring (filename, 0, length - 4)) ^ "-boxed.scm"
		else filename ^ "-boxed.scm"
	     end

       fun main filename
	   = let val parsed_program = Syntax0_parser.main filename
		 val desugared_program = Syntax1_parser.main parsed_program
		 val boxed_program = Syntax2_parser.main desugared_program
		 val out = TextIO.openOut (manufacture_target_name filename)
	     in (write_program (boxed_program, out);
		 TextIO.closeOut out)
	     end

       fun test3 filename
	   = write_program (Syntax2_parser.main (Syntax1_parser.main (Syntax0_parser.main filename)),
			    TextIO.stdOut)

       fun test2 filename
	   = Syntax2_parser.main (Syntax1_parser.main (Syntax0_parser.main filename))

       fun test1 filename
	   = Syntax1_parser.main (Syntax0_parser.main filename)

       fun test0 filename
	   = Syntax0_parser.main filename

       val test = test3
    end
  end;

(* ********** *)

(* end of DAIMI-Scheme.front-end/front-end.sml *)
