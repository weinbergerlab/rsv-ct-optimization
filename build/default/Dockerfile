FROM ubuntu:latest
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages needed to build the image
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y -qq

# These are only needed to build the image (and will not be present in the final image)
ARG BUILD_APT_DEPENDENCIES="libgdal-dev gcc g++ make libcurl4-openssl-dev zlib1g-dev libssl-dev"
RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}

### Setup R packages
ARG R_APT_DEPENDENCIES="libgdal20 r-base  r-cran-littler r-cran-knitr r-cran-reshape r-cran-dplyr r-cran-mgcv r-cran-ggplot2 r-cran-data.table r-cran-tikzdevice"
RUN apt-get install -y -qq --no-install-recommends ${R_APT_DEPENDENCIES}

RUN echo "options(Ncpus = $(nproc --all))" >> /usr/lib/R/etc/Rprofile.site

# These come from CRAN
ARG BUILD_CRAN_DEPENDENCIES="zipcode splitstackshape maps rgdal"
RUN Rscript -e "install.packages(c('docopt', 'remotes'))"
RUN /usr/lib/R/site-library/littler/examples/install2.r ${BUILD_CRAN_DEPENDENCIES}

# These come from GitHub
ARG BUILD_R_GITHUB_DEPENDENCIES="kassambara/easyGgplot2 airbornemint/outbreak-predict"
RUN /usr/lib/R/site-library/littler/examples/installGithub.r ${BUILD_R_GITHUB_DEPENDENCIES}

### Setup TeX packages
ARG TEX_APT_DEPENDENCIES="wget xzdec texlive texlive-lang-cyrillic texlive-latex-extra texlive-fonts-extra texlive-pictures"
RUN apt-get install -y -qq --no-install-recommends ${TEX_APT_DEPENDENCIES}

RUN tlmgr init-usertree
RUN tlmgr --usermode option repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2017/tlnet-final/

ARG BUILD_TEX_DEPENDENCIES="xcharter"
RUN tlmgr --usermode install ${BUILD_TEX_DEPENDENCIES} || echo "updmap errors are okay?!"
RUN updmap-user

### Setup misc other packages needed to run our build
ARG RUN_APT_DEPENDENCIES="tzdata language-pack-en-base gpg git-lfs"
RUN apt-get install -y -qq --no-install-recommends ${RUN_APT_DEPENDENCIES}

# Finish up the configuration
RUN echo tzdata tzdata/Areas select America >> /tmp/debconf.txt
RUN echo tzdata tzdata/Zones/America select New_York >> /tmp/debconf.txt
RUN debconf-set-selections /tmp/debconf.txt
RUN rm /etc/timezone
RUN rm /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata
RUN update-locale --reset LANG=en_US.utf8 LANGUAGE=en_US LC_ALL=en_US.utf8 LC_CTYPE=en_US.utf8

### Cleanup to reduce size of the image
RUN apt-get remove -y -qq ${RUN_APT_DEPENDENCIES}
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm -rf /tmp/*

ENTRYPOINT /bin/bash