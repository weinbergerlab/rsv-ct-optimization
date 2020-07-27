#!/usr/bin/env python3
from pandocfilters import toJSONFilter, walk, RawBlock, RawInline, Plain, Para, Str
from sys import stderr

doublespace = False

def filter(key, value, format, meta):
  global doublespace
  if value in ("[[clearpage]]", "[[cleardoublepage]]"):
    return RawInline("rtf", r"\page")
  elif value == "[[doublespacing]]":
    doublespace = True
    return []
  elif value == "[[singlespacing]]":
    doublespace = False
    return []
  elif key == 'Para':
    return Para([
      RawInline("rtf", r"\sl480\slmult1") if doublespace else RawInline("rtf", r"\sl240\slmult1")
    ] + value)

if __name__ == "__main__":
  toJSONFilter(filter)

