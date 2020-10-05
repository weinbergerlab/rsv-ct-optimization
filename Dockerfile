############################################################
# R with packages and libraries we need
FROM rocker/r-ver:4.0.2 AS build-tools
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
# Please for building

RUN curl https://get.please.build | bash

############################################################
# Tex packages

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
RUN luaotfload-tool -u

############################################################
# R with dependencies for building the paper

WORKDIR /renv

RUN apt-get update -y -qq && apt-get install -y -qq --no-install-recommends \
	gpg \
	git-lfs \
	poppler-utils \
	imagemagick

# Need renv globally to make builds work
RUN install2.r renv

COPY .Rprofile .Rprofile
COPY renv renv
# Devtools before deps to avoid rebuilding devtools if deps change
RUN Rscript -e "renv::install('devtools')"
COPY renv.lock renv.lock
RUN Rscript -e "renv::restore()"

WORKDIR /paper

ENTRYPOINT ["/bin/bash", "-e", "-c"]
