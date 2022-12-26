#!/usr/bin/env python3

import sys

for line in sys.stdin:
    r = line.replace ('@¹', '@1')\
      .replace ('@²', '@2')\
      .replace ('@⁰', '@0')\
      .replace ('⟨*od ', '(list ')\
      .replace ('“', '"')\
      .replace ('”', '"')\
      .replace ('_', ':_')\
      .replace ('⟨int ', '(od-int ')\
      .replace ('⟨void ', '(od-void ')\
      .replace ('⟨char ', '(od-char ')\
      .replace ('⟨varargs ', '(od-varargs ')\
      .replace ('⟨function ', '(od-function ')\
      .replace ('⟨bifunction ', '(od-bifunction ')\
      .replace ('//', ';;')\
      .replace ('𝜏', '%%')\
      .replace ('⟩', ')')
    print (r, end='')
