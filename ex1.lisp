
$g.pushScope ()

;;#include <stdio.h>

  $g.defsynonym identity (od-function "identity"
                                   (list (od-char @1 :_ :_)) ;; param - c
                                   (list (od-char @1 :_ :_))) ;; return type - char
    
     $g.pushScope (identity) 
;;char identity (char c) {
     $g.defsynonym c (od-char @1 param 1)) 
     $s.ensureInScope c 
     $ir.beginFunction identity 
;;  return c;
     $ir.return c
;;}
     $g.popScope () 
     $ir.endFunction identity

;;int main (int argc, char **argv) {
  $g.defsynonym main (od-function "main"
                               (list (od-int @1 :_ :_)(od-char @2 :_ :_)) ;; params - argc, argv
                               (list (od-void :_ :_ :_))) ;; return type - none (void) 
    $g.pushScope (main) 
      $g.defsynonym "argc" (od-int @1 param 1)) 
      $g.defsynonym "argv" (od-char @2 param 2)) 
      $ir.beginFunction "main" 2 
;;  char x = identity ('x');
      $s.ensureInScope x 
      $s.ensureInScope identity 
      $s.ensureType identity (od-function :_ (list (od-char @1 :_ :_)) (list (od-char @1 :_ :_))) 
      $g.defsynonym x (od-char @1 temp 1)) 
      $ir.resetArgs 
      $ir.mutate x (od-char @0 temp "x")  
      $ir.pushArg x
      $ir.defsynonym 𝜏0 (od-char @1 temp 2) 
      $ir.createTemp 𝜏0
      $ir.call identity
      $ir.mutate 𝜏0 (od-char @1 result 1) 
;;  printf ("result = %c\n", x);
      $s.ensureInScope x 
      $s.ensureInScope printf 
      $g.defsynonym printf (od-bifunction :_ (list (od-char @1 :_ :_) (od-varargs :_ :_ :_)) (list (od-void :_ :_ :_))) 
      $s.ensureType printf (od-bifunction :_ (list (od-char @1 :_ :_) (od-varargs :_ :_ :_)) (list (od-void :_ :_ :_))) 
      $ir.resetArgs 
      $ir.defsynonym 𝜏1 (od-char @1 temp 2) 
      $ir.createTemp 𝜏1
      $ir.mutate 𝜏1 (od-char @0 temp "result = %c\n") 
      $ir.pushArg 𝜏1
      $ir.pushArg x 
      $ir.call printf 
      $ir.mutate 𝜏1 (od-char @1 result 1) 
      $ir.return (od-void :_ :_ :_) 
;;}
    $g.popScope 
      $ir.endFunction "main" 2 
