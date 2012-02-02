(* -*- sml -*- ------------------------------------------- *)
(* sexpr.lex                                               *)
(* Sexpression lexical specification for ML-Lex.           *)
(* dOvs 03  -  Mads Sig Ager                               *)
(* ------------------------------------------------------- *)

type ('a,'b) token = ('a,'b) Tokens.token
type pos = int * int
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token

val linenumber = ref 1
val columnnumber = ref 0

fun advance i = columnnumber := (!columnnumber + i)
fun resetcolumn () = (columnnumber := 0)
fun newline () = linenumber := (!linenumber + 1)

fun eof () = (Tokens.EOF((!linenumber,!columnnumber),(!linenumber, !columnnumber)) 
	      before 
	      (linenumber := 1; resetcolumn ()))

(* ------------------------------------------------------- *)
(* %full extends to use character codes 0-255 instead of   *)
(* the default 0-127.  This allows to use Danish           *)
(* characters.                                             *)
(* ------------------------------------------------------- *)

%%

%full 

%header (functor SynLexFun(structure Tokens: Sexp_TOKENS));

letter=[a-zA-Z0-9]|"!"|"$"|"%"|"&"|"*"|"/"|":"|"<" 
       |"="|">"|"?"|"~"|"_"|"^"|"+"|"-"|".";

digit=[0-9];
hex=[0-9a-fA-F];

ws=[\ \t\013];

stringelement=[^\"\\]|"\\\\"|"\\\"";

char=[0-9a-zA-Z]|"!"|"\""|"#"|"$"|"%"|"&"|"'"|"("|")"|"*"|"+"
     |","|"-"|"."|"/"|":"|";"|"<"|"="|">"|"?"|"@"|"["|"\\"
     |"]"|"^"|"_"|"`"|"{"|"|"|"}"|"~"|" "|"æ"|"ø"|"å"
     |"Æ"|"Ø"|"Å";


%%
\n              => (resetcolumn(); newline(); lex());
{ws}+		=> (advance (size yytext); lex());
";"[^\n]*"\n"	=> (resetcolumn(); newline(); continue());
"("		=> (advance (size yytext); 
		    Tokens.LPAREN(((!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));
")"		=> (advance (size yytext); 
		    Tokens.RPAREN(((!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));
"["		=> (advance (size yytext); 
		    Tokens.LBRACKET((!linenumber, !columnnumber),
				    (!linenumber, !columnnumber + size yytext)));
"]"		=> (advance (size yytext); 
		    Tokens.RBRACKET(((!linenumber, !columnnumber),
				     (!linenumber, !columnnumber + size yytext))));
"'"		=> (advance (size yytext); 
		    Tokens.QUOTE(((!linenumber, !columnnumber),
				  (!linenumber, !columnnumber + size yytext))));
"#f"            => (advance (size yytext); 
		    Tokens.FALSE(((!linenumber, !columnnumber), 
				  (!linenumber, !columnnumber + size yytext))));

"#t"            => (advance (size yytext); 
		    Tokens.TRUE(((!linenumber, !columnnumber), 
				 (!linenumber, !columnnumber + size yytext))));

"#x"{hex}+      => (advance (size yytext);
		    Tokens.NUMERAL((let val SOME (i, _) 
					= Int.scan StringCvt.HEX 
					           Substring.getc 
					           (Substring.extract (yytext,2,SOME ((size yytext) - 2)))
				    in i
				    end,
				   (!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));

"-"{digit}+	=> (advance (size yytext); 
		    Tokens.NUMERAL((~(valOf (Int.fromString (substring (yytext, 1, (size yytext) - 1)))),
				   (!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));

"+"{digit}+	=> (advance (size yytext); 
		    Tokens.NUMERAL((valOf (Int.fromString (substring (yytext, 1, (size yytext) - 1))),
				   (!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));

{digit}+	=> (advance (size yytext); 
		    Tokens.NUMERAL((valOf (Int.fromString yytext), 
				   (!linenumber, !columnnumber),
				   (!linenumber, !columnnumber + size yytext))));

{letter}+	=> (advance (size yytext); 
		    Tokens.SYMBOL((yytext, 
				  (!linenumber, !columnnumber), 
				  (!linenumber, !columnnumber + size yytext))));

\"{stringelement}*\" => (advance (size yytext); 
			 Tokens.STRING ((substring (yytext, 1, (size yytext) - 2),
				        (!linenumber, !columnnumber),
					(!linenumber, !columnnumber + size yytext))));

"#\\tab"        => (advance (size yytext); 
		    Tokens.CHARACTER ((#"\t",
				      (!linenumber, !columnnumber),
				      (!linenumber, !columnnumber + size yytext))));

"#\\newline"    => (advance (size yytext); 
		    Tokens.CHARACTER ((#"\n",
				      (!linenumber, !columnnumber),
				      (!linenumber, !columnnumber + size yytext))));

"#\\space"      => (advance (size yytext); 
		    Tokens.CHARACTER ((#" ",
				      (!linenumber, !columnnumber),
				      (!linenumber, !columnnumber + size yytext))));

"#\\"{char}     => (advance (size yytext); 
		    Tokens.CHARACTER ((hd (rev (explode yytext)),
				      (!linenumber, !columnnumber),
				      (!linenumber, !columnnumber + size yytext))));

.               => (print ("*** Parse error line: "^Int.toString(!linenumber)^
			   " column: "^Int.toString(!columnnumber)^
			   " - Illegal DAIMI-scheme syntax: \""^yytext^"\" ***\n"); 
		    linenumber := 1; 
		    resetcolumn ();
		    raise LexError);
