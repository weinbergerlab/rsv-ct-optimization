#!/usr/bin/env python3
from pandocfilters import toJSONFilter, walk, RawBlock, RawInline, Plain, Para, Str
from sys import stderr

doublespace = False

def filter(key, value, format, meta):
  global doublespace
  if value in ("[[clearpage]]", "[[cleardoublepage]]"):
    return RawInline("openxml", r'<w:r><w:br w:type="page"/></w:r>')
  elif value == "[[doublespacing]]":
    doublespace = True
    return []
  elif value == "[[singlespacing]]":
    doublespace = False
    return []
  elif key == 'Para':
    return Para([
      RawInline("openxml", r'<w:pPr><w:spacing w:line="480" w:lineRule="auto"/></w:pPr>') if doublespace else RawInline("openxml", r'<w:pPr><w:spacing w:line="240" w:lineRule="auto"/></w:pPr>')
    ] + value)

if __name__ == "__main__":
  toJSONFilter(filter)

