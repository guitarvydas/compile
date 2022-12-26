#include <stdio.h>

char identity (char c) {
  return c;
}

int main (int argc, char **argv) {
  char x = identity ('x');
  printf ("result = %c\n", x);
}
