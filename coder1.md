# Coder Pass 1
```
#include <stdio.h>

char identity (char c) {
    $ir.functionlabel “identity”
  return c;
    $ir.return ⟨char ⟨dd 1 param “c”⟩⟩
}  
    $ir.functionEnd “identity” 1

int main (int argc, char **argv) {
    $ir.functionlabel “main”
  char x = identity ('x');
    $ir.resetArgs
    $ir.createTemp “𝜏0” ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
    $ir0.initialize ⟨char ⟨dd 0 string 0 _⟩⟩ “x”
    $ir.assignArg 0 ⟨char ⟨dd 0 string 0 _⟩⟩
    $ir.call ⟨dd 1 code “identity” “identity”⟩
    $ir.assignReturnInto 0 ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
  printf ("result = %c\n", x);
    $ir.resetArgs
    $ir.createTemp “𝜏1” ⟨char ⟨dd 1 temp “𝜏1” “𝜏1”⟩⟩
    $ir0.initialize ⟨char ⟨dd 0 string 2 _⟩⟩ “result = %c\n”
    $ir.assignArg 0 ⟨char ⟨dd 0 string 2 _⟩⟩
    $ir.appendArg ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
    $ir.call ⟨dd 1 code “printf” “printf”⟩
    $ir.assignReturnInto 0 ⟨void ⟨dd 0 temp “𝜏1” _⟩⟩
    $ir.returnVoid
}
    $ir.functionEnd “main” 2
```

Combined code:
````

$g.pushScope ()

#include <stdio.h>

  $g.defsynonym identity ⟨function ⟨dd 0 code “identity” “identity”⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩⟩ // param - c
                                   ⟨*dd ⟨char 1 _ _⟩⟩⟩ // return type - char
    $g.pushScope (identity)
char identity (char c) {
      $g.defsynonym c ⟨char ⟨dd 1 param “c”⟩⟩
      $s.ensureInScope c
      $ir.functionlabel “identity”
  return c;
      $ir.return ⟨char ⟨dd 1 param “c”⟩⟩
}  
    $g.popScope ()
    $ir.functionEnd “identity” 1

int main (int argc, char **argv) {
  $g.defsynonym main ⟨function ⟨dd 0 code main 0 1 “main”⟩
                                   ⟨*dd ⟨int 1 _ _⟩⟨char 2 _ _⟩⟩ // params - argc, argv
                                   ⟨*dd ⟨void 0 _ _⟩⟩⟩ // return type - none (void)
    $g.pushScope (main)
      $g.defsynonym argc ⟨int ⟨dd 1 param "argc"⟩⟩
      $g.defsynonym argv ⟨char ⟨dd 2 param "argv"⟩⟩
      $ir.functionlabel “main”
  char x = identity ('x');
      $s.ensureInScope x
      $s.ensureInScope identity
      $s.ensureType identity ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩⟩
                                   ⟨*dd ⟨char 1 _ _⟩⟩⟩
      $g.defsynonym x ⟨char ⟨dd 1 temp "x"⟩⟩
      $ir.resetArgs
      $ir.createTemp “𝜏0” ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
      $ir0.initialize ⟨char ⟨dd 0 string 0 _⟩⟩ “x”
      $ir.assignArg 0 ⟨char ⟨dd 0 string 0 _⟩⟩
      $ir.call ⟨dd 1 code “identity” “identity”⟩
      $ir.assignReturnInto 0 ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
  printf ("result = %c\n", x);
      $s.ensureInScope x
      $s.ensureInScope printf
      $s.ensureType printf ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩ ⟨varargs _ _ _⟩⟩
                                   ⟨*dd ⟨void 0 _ _⟩⟩⟩
      $ir.resetArgs
      $ir.createTemp “𝜏1” ⟨char ⟨dd 1 temp “𝜏1” “𝜏1”⟩⟩
      $ir0.initialize ⟨char ⟨dd 0 string 2 _⟩⟩ “result = %c\n”
      $ir.assignArg 0 ⟨char ⟨dd 0 string 2 _⟩⟩
      $ir.appendArg ⟨char ⟨dd 1 temp “𝜏0” “”⟩⟩
      $ir.call ⟨dd 1 code “printf” “printf”⟩
      $ir.assignReturnInto 0 ⟨void ⟨dd 0 temp “𝜏1” _⟩⟩
      $ir.returnVoid
}
    $g.popScope ()
    $ir.functionEnd “main” 2
   
```