#!/usr/bin/env python3
from pandocfilters import toJSONFilter, walk, RawBlock, RawInline, Plain, Para, Str
from sys import stderr

  if value == "[[clearpage]]":
def filter(key, value, format, meta):
    return RawInline("rtf", r"\page")

if __name__ == "__main__":
  toJSONFilter(filter)
