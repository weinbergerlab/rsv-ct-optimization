FROM ubuntu:latest
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y -qq

# These are needed to build the image (and will not be present in the final image)
ARG DOCKER_APT_DEPENDENCIES="libcurl4-openssl-dev zlib1g-dev libssl-dev wget libgdal-dev gcc r-cran-littler g++"
RUN apt-get install -y -qq --no-install-recommends ${DOCKER_APT_DEPENDENCIES}

# These are needed to run the image # libblas3 libgomp1?
ARG BUILD_APT_DEPENDENCIES="tzdata language-pack-en-base wget xzdec texlive gpg libgdal20 r-base git-lfs"
RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}

# Finish up the configuration
RUN echo tzdata tzdata/Areas select America >> /tmp/debconf.txt
RUN echo tzdata tzdata/Zones/America select New_York >> /tmp/debconf.txt
RUN debconf-set-selections /tmp/debconf.txt
RUN rm /etc/timezone
RUN rm /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata
RUN update-locale --reset LANG=en_US.utf8 LANGUAGE=en_US LC_ALL=en_US.utf8 LC_CTYPE=en_US.utf8

### Setup R packages
RUN echo "options(Ncpus = $(nproc --all))" >> /usr/lib/R/etc/Rprofile.site

# These come from CRAN
ARG BUILD_CRAN_DEPENDENCIES="reshape dplyr mgcv zipcode ggplot2 data.table splitstackshape maps knitr tikzDevice rgdal"
RUN Rscript -e "install.packages(c('docopt', 'remotes'))"
RUN /usr/lib/R/site-library/littler/examples/install2.r ${BUILD_CRAN_DEPENDENCIES}

# These come from GitHub
ARG BUILD_R_GITHUB_DEPENDENCIES="kassambara/easyGgplot2 airbornemint/outbreak-predict"
RUN /usr/lib/R/site-library/littler/examples/installGithub.r ${BUILD_R_GITHUB_DEPENDENCIES}

### Setup TeX packages
RUN tlmgr init-usertree
RUN tlmgr option repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2017/tlnet-final/

ARG BUILD_TEX_DEPENDENCIES="framed printlen pgf draftwatermark everypage lastpage enumitem sectsty xcharter"
RUN tlmgr install ${BUILD_TEX_DEPENDENCIES}

### Cleanup to reduce size of the image
RUN apt-get remove -y -qq ${DOCKER_APT_DEPENDENCIES}
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm -rf /tmp/*

ENTRYPOINT /bin/bash