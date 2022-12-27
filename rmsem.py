#!/usr/bin/env python3

import sys

for line in sys.stdin:
    #if (line.contains ('%s.')):
    if (0 < line.find ('%s.')):
        pass
    else:
        print (line, end='')
