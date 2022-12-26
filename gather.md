# Gathering Information
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
After scanning and parsing, the first step is to gather as much information about the incoming program as possible.  

We want to store the information in some *normalized* manner, to make it easier for a machine to cope with.  

Actually, normalization makes it easier for *us* to explain to the machine what to do, in the small steps that the machine is capable of running.  

If the information is not normalized, our scripts to the machine would contains lots of edge cases.  At some point, the complexity of trying to script a bunch of edge-cases becomes so cumbersome, that we simply give up writing scripts and give up trying to automate the process.

Aside: I will simply ignore the `#include` line.   I want clarity, at this point. Dealing with `#include`  is just more-of-the-same but produces a overwhelming amount of stuff to look at.

So, let's take this simple program apart, line by line, and see what information we can glean fro it.

```
char identity (char c) {
```

This first line appears to occupy only one line of code, but contains multiple pieces of information.

1. The function is called `identity`.
2. The function returns a `char`.
3. The function has one parameter.
4. The parameter is called `c`.
5. The parameter must be of type `char`.
6. A new *scope* is created for the function (by the open brace `{`).

How do we encode all of this information in a normalized way?

# Gathering Information With Declarative Types
We want to record only the facts.  

We want to say `what` exists, not `how` those things are to be implemented.

To declare only the `what` information, we'll use *declarative types*.  We will record the names of the objects and the names of their fields (if any), but, we won't say what type each of those things are.  

For example, we don't need to know that `char` means `int8` in C, we only need to know that there is a type called `char`.

We see that `c` is a parameter and that it can be found in an area reserved for function parameters.  

Where exactly? That's the job of the *allocator* pass.

Can it overlay other objects in memory (memory conservation)?  That's the job of the *optimizer* pass.

For now, we're only concerned with the fact *that* a parameter exists and is be called "c" inside the scope of the function `identity`.

*Declarative types* are a way to record that kind of information in an unbiased manner.  Later, we can refine the information to include where and how many bits are needed.  But, not now.  Later.

So, we want to use a *type* that is called `char`.  It has one field - the position of the beginning bit of the object.  For now, the position will contain only general information, like *this is a slot in "parameter space"*.  Later, we will refine the information by adding more detail.  The knowledge that `char` really means `int8` will be pushed aside and handled at some lower level.  At this point, we care only that there is a type called `char` that is distinct from all of the other types used in the program.

How do we write declarative types?
[[Declarative Types]]

# Annotations For Gathering

Concentrating only on the first function ("identity"), for clarity...

[I'm using the prefix "$g." for operations inserted by the Gatherer pass.  The prefix is unneccessary.  The machine doesn't care, but I find it easier to read.]

1. We create and intersperse `$g.` commands with the original C source
	- subsequent passes can choose to ignore the `$g.` commands
	- subsequent passes parse the C code and insert their own commands into the stream
2. We define a *synonym* for the function named *identity*.  This synonym consists of a *function* declarative type that contains three pieces of information
	- a description of where to find the first bit of the function (in *code* space)
	- a list (\*dd) of descriptors with types for each parameter
	- a list (\*dd) of descriptors with types for each return value
3. push a new (empty) scope
4. create a *synonym* for the parameter `c` 
	- the *synonym* is a Type that contains two pieces of information
		1. the name of the type `char`
		2. the dd (Data Descriptor) which describes where the first bit of the object `c` can be found
			- a slot in the *param* space
			- k=1
				- i.e. 1 level of indirection so data is found inside the slot
		3. pop the scope that was created for the function called *identity* (`}`)

We do the same kind of things for the rest of the code, resulting in
```
$g.pushScope ()

#include <stdio.h>

  $g.defsynonym identity ⟨function ⟨dd 0 code “identity” “identity”⟩ 
                                   ⟨dd* ⟨char 1 _ _⟩⟩ // param - c
                                   ⟨dd* ⟨char 1 _ _⟩⟩ // return type - char
    $g.pushScope (identity)
char identity (char c) {
      $g.defsynonym c ⟨char ⟨dd 1 param “c”⟩⟩
  return c;
}  
    $g.popScope ()

int main (int argc, char **argv) {
  $g.defsynonym main ⟨function ⟨dd 0 code main 0 1 “main”⟩
                                   ⟨dd* ⟨int 1 _ _⟩⟨char 2 _ _⟩⟩ // params - argc, argv
                                   ⟨dd* ⟨void 0 _ _⟩⟩ // return type - none (void)
    $g.pushScope (main)
      $g.defsynonym argc ⟨int ⟨dd 1 param "argc"⟩⟩
      $g.defsynonym argv ⟨char ⟨dd 2 param "argv"⟩⟩
  char x = identity ('x');
      $g.defsynonym x ⟨char ⟨dd 1 temp "x"⟩⟩
  printf ("result = %c\n", x);
}
    $g.popScope ()
```
