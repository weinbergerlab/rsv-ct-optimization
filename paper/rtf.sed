# Change preamble to RTF
s|paper/preamble\.tex|paper/preamble-rtf.tex|
# Drop figures (for now)
s|\\input\{\\figuresDir/(.*)\.tex\}||
# Delete knitr preamble
s/^(\\documentclass[^\\]*)\\.*$/\1/
/\\documentclass/,/end knitr preamble/{
	/\\documentclass/!d
}
# Delete hangparas
s/.*\{hangparas\}.*//