

;;#include <stdio.h>
    
;;char identity (char c) {
     $ir.beginFunction “identity” 1 
;;  return c;
     $ir.return ⟨od char @¹ param 1⟩ 
;;}
     $ir.endFunction “identity” 1

;;int main (int argc, char **argv) {
      $ir.beginFunction “main” 2 
;;  char x = identity ('x');
      $ir.resetArgs 
      $ir.mutate ⟨char @¹ temp 1⟩ ⟨char - temp “x”⟩  
      $ir.pushArg ⟨char @¹ temp 1⟩ 
      $ir.createTemp ⟨char @¹ temp 2⟩ 
      $ir.defsynonym 𝜏0 ⟨char @¹ temp 2⟩ 
      $ir.call ⟨function identity _ _⟩ 
      $ir.mutate 𝜏0 ⟨char @¹ result 1⟩ 
;;  printf ("result = %c\n", x);
      $ir.resetArgs 
      $ir.createTemp ⟨char @¹ temp 2⟩ 
      $ir.defsynonym 𝜏1 ⟨char @¹ temp 2⟩ 
      $ir.mutate ⟨char @¹ string 2⟩ ⟨char - temp “result = %c\n”⟩ 
      $ir.pushArg ⟨char @¹ string 2⟩ 
      $ir.pushArg x 
      $ir.call ⟨function printf_ _⟩ 
      $ir.mutate 𝜏1 ⟨char @¹ result 1⟩ 
      $ir.return ⟨void _ _ _⟩ 
;;}
      $ir.endFunction “main” 2 
