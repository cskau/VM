(* ae.sml *)
(* Standard ML code for the VM course, 24 January 2012 *)
(* Olivier Danvy <danvy@cs.au.dk> *)

structure Source_syntax = 
struct
    datatype rator = PLUS | MINUS | TIMES
    datatype expression = LIT of int
                        | OPR of expression * rator * expression
  end;

structure Semantics
= struct
    datatype value = INT of int
  end;

structure Interpreter
= struct
    fun apply (Source_syntax.PLUS, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 + n2)
      | apply (Source_syntax.MINUS, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 - n2)
      | apply (Source_syntax.TIMES, Semantics.INT n1, Semantics.INT n2)
        = Semantics.INT (n1 * n2)

    fun eval (Source_syntax.LIT n)
        = Semantics.INT n
      | eval (Source_syntax.OPR (rand1, rator, rand2))
        = apply (rator, eval rand1, eval rand2)

    fun main ae
      = eval ae
  end;

structure Target_syntax
= struct
    datatype instruction = PUSH of Semantics.value | ADD | SUB | MUL
    type program = instruction list
  end;

structure Compiler
= struct
    fun translate (Source_syntax.LIT n)
        = [Target_syntax.PUSH (Semantics.INT n)]
      | translate (Source_syntax.OPR (rand1, rator, rand2))
        = (translate rand1) @ (translate rand2) @ (case rator
                                                     of Source_syntax.PLUS
                                                        => [Target_syntax.ADD]
                                                      | Source_syntax.MINUS
                                                        => [Target_syntax.SUB]
                                                      | Source_syntax.TIMES
                                                        => [Target_syntax.MUL])

    fun main ae
        = translate ae
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
       fun decode_execute (PUSH n, s)
           = Stack.push (n, s)
         (* NOTE: arguments are pop'ed in reverse order *)
         | decode_execute (ADD, s)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in Stack.push (Semantics.INT (n1 + n2), s2)
             end
         | decode_execute (SUB, s)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in Stack.push (Semantics.INT (n1 - n2), s2)
             end
         | decode_execute (MUL, s)
           = let val (Semantics.INT n2, s1) = Stack.pop s
                 val (Semantics.INT n1, s2) = Stack.pop s1
             in Stack.push (Semantics.INT (n1 * n2), s2)
             end

       fun loop (nil, s)
           = Stack.pop s
         | loop (i :: is, s)
           = loop (is, decode_execute (i, s))
    end

    fun main p
        = loop (p, Stack.empty_stack)
  end;

structure Test
= struct
    exception RUN_TIME_ERROR

    local open Source_syntax
    in
      (* Test case including all basic operators *)
      val s1 = OPR (LIT 3, TIMES, OPR
                                  (LIT 2, PLUS, OPR (LIT 8, MINUS, LIT 4)))
    end
    (* run expr. through interpreter *)
    val v1 = Interpreter.main s1
    (* compile and run expr. on vm *)
    val t1 = Compiler.main s1
    val w1 = (case Virtual_machine.main t1
                of (w1, nil)
                   => w1
                 | (w1, _)
                   => raise RUN_TIME_ERROR)
    (* compare results from compiler+VM and interpreter *)
    val z1 = if v1 = w1
             then ()
             else raise RUN_TIME_ERROR
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
