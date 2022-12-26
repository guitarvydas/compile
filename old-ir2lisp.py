#!/usr/bin/env python3
#!/usr/bin/env python3

import sys
import json
import html
import re
from string import Template

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
for line in sys.stdin:
    
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
      .tr ('//', ';;')\
      .tr ('𝜏', '%%')\
      .tr ('⟩', ')')
r.pr ()


>>> from string import Template
>>> s = Template('$who likes $what')
>>> s.substitute(who='tim', what='kung pao')
'tim likes kung pao'
>>> d = dict(who='tim')
>>> Template('Give $who $100').substitute(d)
Traceback (most recent call last):
[...]
ValueError: Invalid placeholder in string: line 1, col 10
>>> Template('$who likes $what').substitute(d)
Traceback (most recent call last):
[...]
KeyError: 'what'
>>> Template('$who likes $what').safe_substitute(d)
'tim likes $what'
