# Helper for exporting for Nature Scientific Reports
# - Inlines bibliography
# - Renames images to match the text order

import re
import sys
import os
import shutil

# Inline the bibliography
tex = open(sys.argv[1]).read()

# Get the figure names (in order) and rename the images
# Also pull out captions and labels to build list of figures and update references later
figRE = r'(?s)\\begin\{([^\}]*figure)\}.*?\\end\{[^\}]*figure\}'
graphicRE = r'\\includestandalone\{\\figuresDir/(.*)\}'
captionRE = r'\\caption(?:\[(.*)\])?\{(.*)\}'
labelRE = r'\\label\{(.*)\}'

captions = {}
labels = {}

for figM in re.finditer(figRE, tex):
	m = re.search(graphicRE, figM.group(0))
	figType = figM.group(1)
	captions.setdefault(figType, [])
	labels.setdefault(figType, [])

	figIdx = len(captions[figType]) + 1
	figName = f"Fig{ figIdx }.pdf" if figType == 'figure' else f"SuppFig{ figIdx }.pdf"
	shutil.copy(f'output/figures/{ m.group(1) }.pdf', f'output/figures/{ figName }')

	captions[figType].append(re.search(captionRE, figM.group(0)).group(2))
	labels[figType].append(re.search(labelRE, figM.group(0)).group(1))

# Delete knitr preamble
tex = re.sub(r'(?s)(\\documentclass[^\\]*)\\[^\n]*\n.*%%% end knitr preamble', r'\1\n', tex)

# Migrate figure captions to end of main/supplementary text
tex = re.sub(figRE, '', tex)
lof = r"\section{List of figures}" + '\n\n'.join(fr"\textbf{{Figure {idx + 1}: }}{caption}" for (idx, caption) in enumerate(captions['figure']))
tex = re.sub(r'\\listoffigures', lambda m: lof, tex)

supplof = r"\section{List of supplementary figures}" + '\n\n'.join(fr"\textbf{{Supplementary figure {idx + 1}: }}{caption}" for (idx, caption) in enumerate(captions['supplementaryfigure']))
tex = re.sub(r'\\listofsupplementaryfigures', lambda m: supplof, tex)

# Correct figure references
tex = re.sub(r'\\ref\{(fig:.*?)\}', lambda m: str(labels['figure'].index(m.group(1)) + 1), tex)
tex = re.sub(r'\\ref\{(suppfig:.*?)\}', lambda m: str(labels['supplementaryfigure'].index(m.group(1)) + 1), tex)

# Inline the bibliography
tex = re.sub(r'\\bibliography\{(.*)\}', lambda f: open(f'output/{f.group(1)}.bbl').read(), tex)

open(sys.argv[1], 'w').write(tex)
