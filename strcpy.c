#include <stdio.h>

char *strcpy (char* src, char* dest) {
  while (*dest++ = *src++)
    ;
}

int main (int argc, char**argv) {
  char *s = "Hello";
  char r[20];
  strcpy (s, r);
  printf ("result = %s\n", r);
}

