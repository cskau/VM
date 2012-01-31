(* ea.sml *)
(* Standard ML code for the VM course, 24 January 2012 *)
(* Olivier Danvy <danvy@cs.au.dk> *)

(* A Magritte-style decompiler:
   this is not an integer, it is the representation of an integer.
*)

structure Decompiler
= struct
    local open Target_syntax
    in 
       fun decode_execute (PUSH (Semantics.INT n), s)
           = Stack.push (Source_syntax.LIT (Source_syntax.LIT_INT n), s)
         | decode_execute (ADD, s)
           = let val (e2, s1) = Stack.pop s
                 val (e1, s2) = Stack.pop s1
             in Stack.push (
                    Source_syntax.OPR (e1, Source_syntax.PLUS, e2), s2)
             end
         | decode_execute (SUB, s)
           = let val (e2, s1) = Stack.pop s
                 val (e1, s2) = Stack.pop s1
             in Stack.push (
                    Source_syntax.OPR (e1, Source_syntax.MINUS, e2), s2)
             end
         | decode_execute (MUL, s)
           = let val (e2, s1) = Stack.pop s
                 val (e1, s2) = Stack.pop s1
             in Stack.push (
                    Source_syntax.OPR (e1, Source_syntax.TIMES, e2), s2)
             end
         | decode_execute (DIV, s)
           = let val (e2, s1) = Stack.pop s
                 val (e1, s2) = Stack.pop s1
             in Stack.push (
                    Source_syntax.OPR (e1, Source_syntax.DIVIDE, e2), s2)
             end

       fun loop (nil, s)
           = #1 (Stack.pop s)
         | loop (i :: is, s)
           = loop (is, decode_execute (i, s))
    end

    fun main p
        = loop (p, Stack.empty_stack)
  end;

structure Test'
= struct
    exception RUN_TIME_ERROR

    local open Source_syntax
    in
       val s1 = OPR (LIT (LIT_INT 3), 
                     TIMES, 
                     OPR (LIT (LIT_INT 2), PLUS, LIT (LIT_INT 4)))
    end
    val t1 = Compiler.main s1
    val w1 = Decompiler.main t1
    val z1 = if s1 = w1
             then ()
             else raise RUN_TIME_ERROR

    local open Source_syntax
    in
       val s2 = OPR (OPR (LIT (LIT_INT 30), 
                          TIMES, 
                          OPR (LIT (LIT_INT 20), PLUS, LIT (LIT_INT 40))),
                     TIMES,
                     OPR (LIT (LIT_INT 2), 
                          PLUS, 
                          OPR (LIT (LIT_INT 4), DIVIDE, LIT (LIT_INT 1))))
    end
    val t2 = Compiler.main s2
    val w2 = Decompiler.main t2
    val z2 = if s2 = w2
             then ()
             else raise RUN_TIME_ERROR
  end;

(* ********** *)

(*
   Exercise 3' (a continuation of Exercise 3):

   Having extended the source syntax and the target syntax with division,
   extend the decompiler.
*)

(* end of ea.sml *)
