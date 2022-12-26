# Semantics Checking
After information has been gathered, the rest of the incoming code can be checked for consistency.

We simply skip over any incoming code that declares information and look at only the incoming code that performs work.

In general, we use the parser to build a CST[cst] for us, then tree-walk the CST, looking only for nodes of interest and ignoring other kinds of nodes.

[^cst]: A CST - Concrete Syntax Tree - is the culled version of the more general AST - Abstract Syntax Tree.  The AST defines all of the *possibilities*, whereas the CST represents what is really there and discards possibilities that weren't needed.  Notably, an AST might contain nodes that say something like "one or more of this type of thing", whereas the CST has exactly one node for each thing that was actually found in the input.  A CST doesn't contain generalizations like "one of more of".

In a process similar to that in the *gathering* phase, the semantic checker inserts commands into the input stream at the appropriate points.  It simply ignores syntax that doesn't apply to syntax checking.  The semantic pass simply forwards ignored code to downstream passes.

Again, for my own readability, I choose to use the prefix `$s.` for semantic commands inserted by this pass.  The machine doesn't care, but, it makes debugging this stuff easier for me if I can visually inspect it and not have to think too hard about what I'm looking at.

```
#include <stdio.h>

char identity (char c) {
  return c;
}  

int main (int argc, char **argv) {
  char x = identity ('x');
  printf ("result = %c\n", x);
}
```

Looking at the original C code, we see that only a few checks are needed:
- check that `c` is a variable within the scope of the function `identity` (using `ensureInScope`)
- check that `x` is a variable within the scope of the function `main` (using `ensureInScope`)
- check that `identity` is a function with the appropriate signature (input=one char, output=one char)
- check that `main` is a function with the appropriate signature (input=one char and a variable number of args, output=void)

```
#include <stdio.h>
char identity (char c) {
      $s.ensureInScope c
  return c;
}  

int main (int argc, char **argv) {
  char x = identity ('x');
      $s.ensureInScope x
      $s.ensureInScope identity
      $s.ensureType identity ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩⟩
                                   ⟨*dd ⟨char 1 _ _⟩⟩⟩
  printf ("result = %c\n", x);
      $s.ensureInScope x
      $s.ensureInScope printf
      $s.ensureType printf ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩ ⟨varargs _ _ _⟩⟩
                                   ⟨*dd ⟨void 0 _ _⟩⟩⟩
}

```

The combined result is:
```

$g.pushScope ()

#include <stdio.h>

  $g.defsynonym identity ⟨function ⟨dd 0 code “identity” “identity”⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩⟩ // param - c
                                   ⟨*dd ⟨char 1 _ _⟩⟩⟩ // return type - char
    $g.pushScope (identity)
char identity (char c) {
      $g.defsynonym c ⟨char ⟨dd 1 param “c”⟩⟩
      $s.ensureInScope c
  return c;
}  
    $g.popScope ()

int main (int argc, char **argv) {
  $g.defsynonym main ⟨function ⟨dd 0 code main 0 1 “main”⟩
                                   ⟨*dd ⟨int 1 _ _⟩⟨char 2 _ _⟩⟩ // params - argc, argv
                                   ⟨*dd ⟨void 0 _ _⟩⟩⟩ // return type - none (void)
    $g.pushScope (main)
      $g.defsynonym argc ⟨int ⟨dd 1 param "argc"⟩⟩
      $g.defsynonym argv ⟨char ⟨dd 2 param "argv"⟩⟩
  char x = identity ('x');
      $s.ensureInScope x
      $s.ensureInScope identity
      $s.ensureType identity ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩⟩
                                   ⟨*dd ⟨char 1 _ _⟩⟩⟩
      $g.defsynonym x ⟨char ⟨dd 1 temp "x"⟩⟩
  printf ("result = %c\n", x);
      $s.ensureInScope x
      $s.ensureInScope printf
      $s.ensureType printf ⟨function ⟨dd 0 code _ _⟩ 
                                   ⟨*dd ⟨char 1 _ _⟩ ⟨varargs _ _ _⟩⟩
                                   ⟨*dd ⟨void 0 _ _⟩⟩⟩
}
    $g.popScope ()

```
