# Helper for exporting for AAP pediatrics
# - Inlines bibliography
# - Renames images to match the text order
# - Rewrites a few other things to make conversion to RTF work

import re
import sys
import os
import shutil

# Inline the bibliography
tex = open(sys.argv[1]).read()
# tex = re.sub(r'\\bibliography\{(.*)\}', lambda f: open(f'{f.group(1)}.bbl').read(), tex)

# Get the figure names (in order) and rename the images
# Also pull out captions and labels to build list of figures and update references later
figRE = r'(?s)\\begin\{([^\}]*figure)\}.*?\\end\{[^\}]*figure\}'
graphicRE = r'\\includestandalone\{\\figuresDir/(.*)\}'
captionRE = r'\\caption(?:\[(.*)\])?\{(.*)\}'
labelRE = r'\\label\{(.*)\}'

captions = {}
labels = {}

for figIdx, figM in enumerate(re.finditer(figRE, tex)):
	m = re.search(graphicRE, figM.group(0))
	figType = figM.group(1)
	shutil.copy(f'output/figures/{ m.group(1) }.pdf', f'output/figures/Fig{ figIdx + 1 }.pdf')
	captions.setdefault(figType, []).append(re.search(captionRE, figM.group(0)).group(2))
	labels.setdefault(figType, []).append(re.search(labelRE, figM.group(0)).group(1))

# Change preamble to RTF
tex = tex.replace(r'paper/preamble.tex', r'paper/preamble-rtf.tex')
# Drop graphics from figures
tex = re.sub(graphicRE, r'', tex)
# Delete knitr preamble
tex = re.sub(r'(?s)(\\documentclass[^\\]*)\\[^\n]*\n.*%%% end knitr preamble', r'\1\n', tex)

# Migrate figure captions to end of main/supplemental text
tex = re.sub(figRE, '', tex)
lof = '\n\n'.join(fr"\textbf{{Figure {idx + 1}: }}{caption}" for (idx, caption) in enumerate(captions['figure']))
tex = re.sub(r'\\listoffigures', lambda m: lof, tex)

supplof = '\n\n'.join(fr"\textbf{{Supplemental figure {idx + 1}: }}{caption}" for (idx, caption) in enumerate(captions['supplementalfigure']))
tex = re.sub(r'\\listofsupplementalfigures', lambda m: supplof, tex)


# Correct figure references
tex = re.sub(r'\\ref\{(fig:.*?)\}', lambda m: str(labels['figure'].index(m.group(1)) + 1), tex)
tex = re.sub(r'\\ref\{(suppfig:.*?)\}', lambda m: str(labels['supplementalfigure'].index(m.group(1)) + 1), tex)

# Inline the bibliography
tex = re.sub(r'\\bibliography\{(.*)\}', lambda f: open(f'output/{f.group(1)}.bbl').read(), tex)
tex = re.sub(r'(?s)\\begin\{thebibliography\}\{.*?\}(.*)\\end\{thebibliography\}', r'\\section*{References}\n\1\n', tex)

# Rewrite citations by turning each into superscript number
# First identify them and figure out the order
citeRE = r'\\cite\{(.*?)\}'
citations = []
for citeM in re.finditer(citeRE, tex):
	for cite in citeM.group(1).split(','):
		if cite not in citations:
			citations.append(cite)

for idx, cite in enumerate(citations):
	citationRE = fr'(\\cite\{{(?:[^\{{\}}]+,)?){ re.escape(cite) }((?:,[^\{{\}}]*?)?\}})'
	tex = re.sub(citationRE, lambda m: f'{ m.group(1) }{ idx + 1 }{ m.group(2) }', tex)
	bibRE = fr'(\\bibitem\{{){ re.escape(cite) }(\}})'
	tex = re.sub(bibRE, lambda m: f'[{ idx + 1 }]', tex)

tex = re.sub(citeRE, r'\\textsuperscript{\1}', tex)

# Encode page breaks and spacing so that filter can output rtf commands later
tex = re.sub(r'\\(clear(?:double)?page|(:?double|single)spacing)', lambda m: rf'[[{m.group(1)}]]', tex)

# Misc
tex = tex.replace(r'\raisebox{1pt}{\Circle}', r'\Circle')

open(sys.argv[1], 'w').write(tex)
