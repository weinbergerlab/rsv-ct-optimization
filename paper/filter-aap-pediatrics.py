#!/usr/bin/env python3
from pandocfilters import toJSONFilter, walk, RawBlock, RawInline, Plain, Para, Str
from sys import stderr

def behead(key, value, format, meta):
  if value == "[[clearpage]]":
    return RawInline("rtf", r"\page")

if __name__ == "__main__":
  toJSONFilter(behead)
