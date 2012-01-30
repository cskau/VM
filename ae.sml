(* ae.sml *)
(* Standard ML code for the VM course, 24 January 2012 *)
(* Olivier Danvy <danvy@cs.au.dk> *)

structure Source_syntax = 
struct
    datatype rator = PLUS | MINUS | TIMES | DIVIDE | EQUAL
    datatype lit = LIT_INT of int
                 | LIT_BOOL of bool
    datatype expression = LIT of lit
                        | OPR of expression * rator * expression
                        | IF0 of expression * expression * expression
  end;

structure Semantics
= struct
    exception TYPE_ERROR

    datatype value = INT of int
                   | BOOL of bool
  end;

structure Interpreter
= struct
    fun apply (Source_syntax.PLUS, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 + n2)
      | apply (Source_syntax.MINUS, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 - n2)
      | apply (Source_syntax.TIMES, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 * n2)
      | apply (Source_syntax.DIVIDE, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 div n2)
      | apply (Source_syntax.EQUAL, Semantics.INT n1, Semantics.INT n2)
        = Semantics.BOOL (n1 = n2)
      | apply (Source_syntax.EQUAL, Semantics.BOOL n1, Semantics.BOOL n2)
        = Semantics.BOOL (n1 = n2)

    fun eval (Source_syntax.LIT (Source_syntax.LIT_INT n))
        = Semantics.INT n
      | eval (Source_syntax.LIT (Source_syntax.LIT_BOOL n))
        = Semantics.BOOL n
      | eval (Source_syntax.OPR (rand1, rator, rand2))
        = apply (rator, eval rand1, eval rand2)
      | eval (Source_syntax.IF0 (v, e1, e2))
        = if (eval v) = Semantics.INT 0
             then eval e1
             else eval e2

    fun main ae
      = eval ae
  end;

structure Target_syntax
= struct
    (* TODO: Make label IDs a seperate, unique type *)
    datatype instruction = PUSH of Semantics.value
                         | ADD | SUB | MUL | DIV | CMP
                         | JMP of Semantics.value
                         | IF0 of Semantics.value
                         | LBL of Semantics.value
    type program = instruction list
  end;

structure Compiler
= struct
    (* very simple "next label" generator *)
    fun genlbl (Semantics.INT l)
        = (Semantics.INT (l + 1))

    (* right-branch and fill acc *)
    fun translate (Source_syntax.LIT (Source_syntax.LIT_INT n), acc, l)
        = (Target_syntax.PUSH (Semantics.INT n)) :: acc
      | translate (Source_syntax.LIT (Source_syntax.LIT_BOOL n), acc, l)
        = (Target_syntax.PUSH (Semantics.BOOL n)) :: acc
      | translate (Source_syntax.OPR (rand1, rator, rand2), acc, l)
        = let val acc1 = (case rator
                           of Source_syntax.PLUS
                              => (Target_syntax.ADD) :: acc
                            | Source_syntax.MINUS
                              => (Target_syntax.SUB) :: acc
                            | Source_syntax.TIMES
                              => (Target_syntax.MUL) :: acc
                            | Source_syntax.DIVIDE
                              => (Target_syntax.DIV) :: acc
                            | Source_syntax.EQUAL
                              => (Target_syntax.CMP) :: acc)
              val acc2 = translate (rand2, acc1, l)
          in translate (rand1, acc2, l)
          end
      | translate (Source_syntax.IF0 (v, e1, e2), acc, l)
        (* translation: IF0 LBL1, e2, JMP LBL2, LBL1, e1, LBL2 *)
        = let val l1 = (genlbl l)
              val l2 = (genlbl l1)
              val acc1 = (Target_syntax.LBL l2) :: acc
              val acc2 = translate (e1, acc1, l2)
              val acc3 = (Target_syntax.LBL l1) :: acc2
              val acc4 = (Target_syntax.JMP l2) :: acc3
              val acc5 = translate (e2, acc4, l2)
              val acc6 = (Target_syntax.IF0 l1) :: acc5
          in
              translate (v, acc6, l2)
          end

    (* Give nil (empty list) as initial accumulation *)
    fun main ae
        = translate (ae, nil, (Semantics.INT 0))
  end;

structure Stack
= struct
    type 'a stack = 'a list

    exception EMPTY_STACK

    val empty_stack = nil

    fun is_empty nil
        = true
      | is_empty (_ :: _)
        = false

    fun push (x, xs)
        = x :: xs

    fun pop nil
        = raise EMPTY_STACK
      | pop (x :: xs)
        = (x, xs)
  end;

structure Virtual_machine
= struct
    local open Target_syntax
    in
       fun jump (n, s, i :: is)
           = if (LBL n) = i
             then (is, s)
             (* loop until we find the label we're looking for *)
             else (jump (n, s, is))
         | jump (n, s, nil)
           = (nil, s)

       fun decode_execute (PUSH n, s, is)
           = (is, Stack.push (n, s))
         (* NOTE: arguments are pop'ed in reverse order *)
         | decode_execute (ADD, s, is)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in (is, Stack.push (Semantics.INT (n1 + n2), s2))
             end
         | decode_execute (SUB, s, is)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in (is, Stack.push (Semantics.INT (n1 - n2), s2))
             end
         | decode_execute (MUL, s, is)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in (is, Stack.push (Semantics.INT (n1 * n2), s2))
             end
         | decode_execute (DIV, s, is)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in (is, Stack.push (Semantics.INT (n1 div n2), s2))
             end
         | decode_execute (CMP, s, is)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in (is, Stack.push (Semantics.BOOL (n1 = n2), s2))
             end
         | decode_execute (LBL n, s, is)
           = (is, s)
         | decode_execute (JMP n, s, i :: is)
           = (jump (n, s, is))
         | decode_execute (IF0 n, s, i :: is)
           = let val (Semantics.INT v, s1) = Stack.pop s
             in
               if v = 0
               then (jump (n, s1, is))
               else (i :: is, s1)
             end

       fun loop (nil, s)
           = Stack.pop s
         | loop (i :: is, s)
           = loop (decode_execute (i, s, is))
    end

    fun main p
        = loop (p, Stack.empty_stack)
  end;

structure Test
= struct
    exception RUN_TIME_ERROR

    (* compare results from compiler+VM and interpreter *)
    fun test tc
      = let val ir = (Interpreter.main tc)
            val vmr = (case Virtual_machine.main (Compiler.main tc)
                         of (w1, nil)
                            => w1
                          | (w1, _)
                            => raise RUN_TIME_ERROR)
        in
          if ir = vmr
          then (ir, vmr)
          else raise RUN_TIME_ERROR
        end

    local open Source_syntax
    in
      val tc1 = test (OPR (LIT (LIT_INT 3),
                           TIMES,
                           OPR (LIT (LIT_INT 2),
                                PLUS,
                                LIT (LIT_INT 4))))

      val tc2 = test (OPR (LIT (LIT_INT 3),
                           TIMES,
                           OPR (LIT (LIT_INT 4), MINUS, LIT (LIT_INT 2))))

      val tc3 = test (OPR (LIT (LIT_INT 6),
                           DIVIDE,
                           OPR (LIT (LIT_INT 2), PLUS, LIT (LIT_INT 4))))

      val tc4 = test (IF0 (LIT (LIT_INT 0), LIT (LIT_INT 1), LIT (LIT_INT 2)))

      val tc5 = test (IF0 (OPR (LIT (LIT_INT ~1), MINUS, LIT (LIT_INT ~1)),
                           OPR (LIT (LIT_INT 2), PLUS, LIT (LIT_INT 4)),
                           OPR (LIT (LIT_INT 2), TIMES, LIT (LIT_INT 4))))

      val tc6 = test (LIT (LIT_BOOL true))

      val tc7 = test (OPR (LIT (LIT_INT ~1), EQUAL, LIT (LIT_INT 1)))
    end
  end;

(* ********** *)

(*
   Exercise 1:

   The virtual machine contains a bug.  Find it and fix it.
*)

(*
   Exercise 2:

   The compiler uses list concatenation (ie, append, noted ``@'').
   Rewrite it so that it does not use append but cons (noted ``::'').
   (Hint: use an accumulator.)
*)

(*
   Exercise 3:

   Extend the source syntax and the target syntax with division,
   and then the interpreter, the compiler, and the virtual machine.
*)

(* Exercises 4 and 5 are to be made independently of each other. *)

(*
   Exercise 4:

   Extend the source syntax with a conditional expression
   and the interpreter
   so as evaluating
     IF0 (INT 0, e1, e2)
   will lead to e1 being evaluated,
   and evaluating
     IF0 (v, e1, e2)
   will lead to e2 being evaluated if v is not INT 0.

   Extend the target syntax, the compiler, and the virtual machine
   to conditional expressions.
   (Hint: you will need to add labels.)

*)

(*
   Exercise 5:

   Extend the source syntax with booleans,
   and add a comparison operator.

structure Source_syntax
= struct
    datatype rator = PLUS | MINUS | TIMES | EQUAL
    datatype lit = LIT_INT of int
                 | LIT_BOOL of bool
    datatype expression = LIT of lit
                        | OPR of expression * rator * expression
  end;

   Then extend the interpreter, the compiler, and the virtual machine.
   (Hint: you will need to add
      exception TYPE_ERROR
   in the Semantics module.)
*)

(*
   Exercise 6 (the combination of Exercises 4 and 5):

   Extend the source syntax with booleans,
   a comparison operator, and
   a boolean conditional expression.

   Then extend the interpreter, the compiler, and the virtual machine.
*)

(* end of ae.sml *)
