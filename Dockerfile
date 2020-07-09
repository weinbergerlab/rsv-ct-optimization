############################################################
# R with packages and libraries we need
FROM rocker/r-ver AS r
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages needed to build the image
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install -y --no-install-recommends \
	libgdal-dev \
	libcurl4-openssl-dev \
	zlib1g-dev \
	libssl-dev curl \
	libgdal26

############################################################
# Tex environment we use to build the paper (it includes R because of knitr)
FROM r AS tex

RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends \
	wget \
	xzdec \
	texlive \
	texlive-binaries \
	texlive-luatex \
	texlive-lang-cyrillic \
	texlive-latex-extra \
	texlive-bibtex-extra \
	texlive-fonts-extra \
	texlive-pictures \
	lmodern \
	latexmk

RUN tlmgr init-usertree
RUN tlmgr --usermode option repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2019/tlnet-final/

# Something's funy with tlmgr's call to updmap and I don't want to debug it today; it succeeds if run twice
ARG TEX_PACKAGES=xcharter
RUN tlmgr install ${TEX_PACKAGES} || tlmgr install ${TEX_PACKAGES}
RUN updmap-user

ENTRYPOINT ["/bin/bash", "-c"]

############################################################
# R with dependencies for building the paper
FROM tex AS paper-tools

WORKDIR /paper

RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends \
	gpg \
	git-lfs \
	pandoc \
	poppler-utils \
	imagemagick

COPY .Rprofile .Rprofile
COPY renv renv
# Devtools before deps to avoid rebuilding devtools if deps change
RUN Rscript -e "renv::install('devtools')"
COPY renv.lock renv.lock
RUN Rscript -e "renv::restore()"

COPY . .

ENTRYPOINT ["/bin/bash", "-e", "-c"]
