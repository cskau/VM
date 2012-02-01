(* DAIMI-Scheme.front-end/gensym.sml *)
(* a generator of fresh names        *)
(* Olivier Danvy <danvy@brics.dk>    *)
(* October 2003                      *)

(* ********** *)

structure Gensym
= struct
    local val n = ref ~1
    in fun init () = n := ~1
       fun new base = (n := !n + 1; base ^ Int.toString (!n))
    end
  end;

(* ********** *)

(* end of DAIMI-Scheme.front-end/gensym.sml *)
