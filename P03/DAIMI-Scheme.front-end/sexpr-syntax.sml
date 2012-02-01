(* sexpr-syntax.sml -*- sml -*-       *)
(* DAIMI Scheme S-expressions         *)
(* Olivier Danvy <danvy@brics.dk>     *)
(* October 2003                       *)

(* ********** *)

signature SEXPR
= sig
    datatype sexpr = INTEGER of int
                   | BOOLEAN of bool
                   | CHARACTER of char
                   | STRING of string
                   | SYMBOL of string
                   | NIL
                   | PAIR of sexpr * sexpr

    type program = sexpr list

    val print_program : program -> unit
    val print : sexpr -> unit
    val print_out : sexpr * TextIO.outstream -> unit
  end;

structure Sexpr : SEXPR
= struct
    datatype sexpr = INTEGER of int
                   | BOOLEAN of bool
                   | CHARACTER of char
                   | STRING of string
                   | SYMBOL of string
                   | NIL
                   | PAIR of sexpr * sexpr

    type program = sexpr list

    exception NON_NIL_CDR of sexpr

    fun print_out (e, out)
	= let fun write_cdr NIL
                  = TextIO.output (out, ")")
		| write_cdr (PAIR (e1, e2))
		  = (TextIO.output (out, " ");
		     write_e e1;
		     write_cdr e2)
		| write_cdr e
		  = (TextIO.output (out, "\n");
		     raise (NON_NIL_CDR e))
	   and write_e (INTEGER n)
		  = TextIO.output (out, Int.toString n)
		| write_e (BOOLEAN true)
		  = TextIO.output (out, "#t")
		| write_e (BOOLEAN false)
		  = TextIO.output (out, "#f")
		| write_e (CHARACTER c)
		  = (TextIO.output (out, "#\\");
		     TextIO.output (out, Char.toString c))
		| write_e (SYMBOL s)
		  = TextIO.output (out, s)
		| write_e NIL
		  = TextIO.output (out, "'()")
		| write_e (PAIR (e1, e2))
		  = (TextIO.output (out, "(");
		     write_e e1;
		     write_cdr e2)
		| write_e (STRING s)
		  = (TextIO.output (out, "\"");
		     TextIO.output (out, s);
		     TextIO.output (out, "\""))
	  in write_e e
	  end

    fun print_program nil
        = ()
      | print_program (e :: es)
	= (print_out (e, TextIO.stdOut);
	   TextIO.output (TextIO.stdOut, "\n");
	   print_program es)

    fun print e
	= print_out (e, TextIO.stdOut)
  end;

(* ********** *)

(* end of sexpr-syntax.sml *)
