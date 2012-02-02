(* -*- sml -*--------------------------------------------- *)
(* sexpr-parser.sml                                        *)
(* Sexpression parser interface.                           *)
(* dOvs 03  -  Mads Sig Ager                               *)
(* ------------------------------------------------------- *)

structure SynLrVals
= SynLrValsFun (structure Token = LrParser.Token
	        structure Sexpr = Sexpr);

structure SynLex
= SynLexFun (structure Tokens = SynLrVals.Tokens);

structure SynParser
= Join (structure LrParser = LrParser
	structure ParserData = SynLrVals.ParserData
	structure Lex = SynLex);

signature SEXPR_PARSER
= sig
    exception Parse_Error

    val read_file : string -> Sexpr.program
  end;

structure Sexpr_parser : SEXPR_PARSER
= struct
    exception Parse_Error

    fun inputc a b
	= TextIO.inputN (a, b)

    fun read_file s
	= let val the_input_stream
		  = TextIO.openIn s
	      val ll
		  = SynParser.makeLexer (inputc the_input_stream)
	      fun complain (s:string,pos1,pos2)
		  = (case pos1
		       of (line, column)
			  => print ("*** Parse error line: "^Int.toString line^
				    " column: "^Int.toString column^
				    " - Illegal DAIMI-scheme syntax: \""^s^"\" ***\n"))
	      val result
		  = #1 (SynParser.parse(0,ll,complain,()))
                    handle ParseError
		           => (TextIO.output (TextIO.stdOut, "\n");
			       TextIO.output (TextIO.stdOut, s);
			       TextIO.output (TextIO.stdOut, "\n");
			       raise ParseError)
	      val _
		  = TextIO.closeIn the_input_stream
	  in result
	  end
  end;

(* end of sexpr-parser.sml *)
