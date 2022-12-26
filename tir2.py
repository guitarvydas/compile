#!/usr/bin/env python3

import sys
import json
import html
import re

class LineCommand:
    def __init__ (self, fname):
        with open(sys.argv[1]) as fp:
            self.Lines = fp.readlines()

    def tr (self, pattern, replacement):
        r = []
        for line in self.Lines:
            rline = line.replace (pattern, replacement)
            r.append (rline)
        self.Lines = r
        return self

    def pr (self):
        for line in self.Lines:
            print (line, end='')

lc = LineCommand (sys.argv [1])    
r = lc.tr ('@¹', '@1')\
      .tr ('@²', '@2')\
      .tr ('@⁰', '@0')\
      .tr ('⟨*od ', '(list ')\
      .tr ('“', '"')\
      .tr ('”', '"')\
      .tr ('_', ':_')\
      .tr ('⟨int ', '(od-int ')\
      .tr ('⟨void ', '(od-void ')\
      .tr ('⟨char ', '(od-char ')\
      .tr ('⟨varargs ', '(od-varargs ')\
      .tr ('⟨function ', '(od-function ')\
      .tr ('⟨bifunction ', '(od-bifunction ')\
      .tr ('⟩', ')')
r.pr ()
