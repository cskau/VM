
functor SynLrValsFun(structure Token: TOKEN
			      structure Sexpr : SEXPR) = 
struct
structure ParserData=
struct
structure Header = 
struct
(* -*- sml -*- ------------------------------------------- *)
(* sexpr.grm                                               *)
(* Grammar of Sexpressions.  ML-Yacc specification.        *)
(* dOvs 03  -  Mads Sig Ager                               *)
(* ------------------------------------------------------- *)

fun build_list (nil)
    = Sexpr.NIL
  | build_list (sexpr :: sexprs)
    = Sexpr.PAIR (sexpr, build_list sexprs)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\004\000\011\000\006\000\010\000\
\\007\000\009\000\008\000\008\000\009\000\007\000\010\000\006\000\
\\012\000\005\000\000\000\
\\001\000\005\000\019\000\000\000\
\\001\000\011\000\018\000\000\000\
\\021\000\000\000\
\\022\000\000\000\
\\023\000\002\000\013\000\003\000\012\000\004\000\011\000\006\000\010\000\
\\007\000\009\000\008\000\008\000\009\000\007\000\010\000\006\000\
\\012\000\005\000\000\000\
\\024\000\000\000\
\\025\000\000\000\
\\026\000\000\000\
\\027\000\000\000\
\\028\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\"
val actionRowNumbers =
"\006\000\006\000\004\000\006\000\
\\001\000\011\000\008\000\009\000\
\\010\000\006\000\007\000\012\000\
\\005\000\003\000\015\000\002\000\
\\014\000\013\000\000\000"
val gotoT =
"\
\\001\000\018\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\012\000\003\000\001\000\000\000\
\\000\000\
\\002\000\013\000\003\000\001\000\000\000\
\\003\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\015\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 19
val numrules = 12
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int*int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | CHARACTER of  (char)
 | NUMERAL of  (int) | STRING of  (string) | SYMBOL of  (string)
 | sexpr of  (Sexpr.sexpr) | sexprlist of  (Sexpr.sexpr list)
 | program of  (Sexpr.program)
end
type svalue = MlyValue.svalue
type result = Sexpr.program
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "SYMBOL"
  | (T 2) => "STRING"
  | (T 3) => "LPAREN"
  | (T 4) => "RPAREN"
  | (T 5) => "NUMERAL"
  | (T 6) => "FALSE"
  | (T 7) => "TRUE"
  | (T 8) => "CHARACTER"
  | (T 9) => "QUOTE"
  | (T 10) => "RBRACKET"
  | (T 11) => "LBRACKET"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 3) :: (T 4) :: (T 6) :: (T 7) :: (T 9) :: (T 
10) :: (T 11) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.sexprlist sexprlist,sexprlist1left,sexprlist1right)
)::rest671) => let val result=MlyValue.program((sexprlist))
 in (LrTable.NT 0,(result,sexprlist1left,sexprlist1right),rest671) end
| (1,(_,(MlyValue.sexprlist sexprlist,_,sexprlist1right))::(_,(
MlyValue.sexpr sexpr,sexpr1left,_))::rest671) => let val result=
MlyValue.sexprlist((sexpr :: sexprlist))
 in (LrTable.NT 1,(result,sexpr1left,sexprlist1right),rest671) end
| (2,rest671) => let val result=MlyValue.sexprlist((nil))
 in (LrTable.NT 1,(result,defaultPos,defaultPos),rest671) end
| (3,(_,(MlyValue.STRING STRING,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.sexpr((Sexpr.STRING STRING))
 in (LrTable.NT 2,(result,STRING1left,STRING1right),rest671) end
| (4,(_,(_,TRUE1left,TRUE1right))::rest671) => let val result=
MlyValue.sexpr((Sexpr.BOOLEAN true))
 in (LrTable.NT 2,(result,TRUE1left,TRUE1right),rest671) end
| (5,(_,(_,FALSE1left,FALSE1right))::rest671) => let val result=
MlyValue.sexpr((Sexpr.BOOLEAN false))
 in (LrTable.NT 2,(result,FALSE1left,FALSE1right),rest671) end
| (6,(_,(MlyValue.NUMERAL NUMERAL,NUMERAL1left,NUMERAL1right))::
rest671) => let val result=MlyValue.sexpr((Sexpr.INTEGER NUMERAL))
 in (LrTable.NT 2,(result,NUMERAL1left,NUMERAL1right),rest671) end
| (7,(_,(MlyValue.CHARACTER CHARACTER,CHARACTER1left,CHARACTER1right))
::rest671) => let val result=MlyValue.sexpr((Sexpr.CHARACTER CHARACTER
))
 in (LrTable.NT 2,(result,CHARACTER1left,CHARACTER1right),rest671) end
| (8,(_,(MlyValue.SYMBOL SYMBOL,SYMBOL1left,SYMBOL1right))::rest671)
 => let val result=MlyValue.sexpr((Sexpr.SYMBOL SYMBOL))
 in (LrTable.NT 2,(result,SYMBOL1left,SYMBOL1right),rest671) end
| (9,(_,(_,_,RPAREN1right))::(_,(MlyValue.sexprlist sexprlist,_,_))::(
_,(_,LPAREN1left,_))::rest671) => let val result=MlyValue.sexpr((
build_list sexprlist))
 in (LrTable.NT 2,(result,LPAREN1left,RPAREN1right),rest671) end
| (10,(_,(_,_,RBRACKET1right))::(_,(MlyValue.sexprlist sexprlist,_,_))
::(_,(_,LBRACKET1left,_))::rest671) => let val result=MlyValue.sexpr((
build_list sexprlist))
 in (LrTable.NT 2,(result,LBRACKET1left,RBRACKET1right),rest671) end
| (11,(_,(MlyValue.sexpr sexpr,_,sexpr1right))::(_,(_,QUOTE1left,_))::
rest671) => let val result=MlyValue.sexpr((
Sexpr.PAIR (Sexpr.SYMBOL "quote", Sexpr.PAIR (sexpr, Sexpr.NIL))))
 in (LrTable.NT 2,(result,QUOTE1left,sexpr1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Sexp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun SYMBOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.SYMBOL i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.STRING i,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.NUMERAL i,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun CHARACTER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.CHARACTER i,p1,p2))
fun QUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
end
end
