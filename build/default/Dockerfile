FROM ubuntu:latest
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y -qq

# These are needed to build the image (and will not be present in the final image)
ENV DOCKER_APT_DEPENDENCIES="libcurl4-openssl-dev zlib1g-dev libssl-dev wget libgdal-dev gcc"
RUN apt-get install -y -qq --no-install-recommends ${DOCKER_APT_DEPENDENCIES}

# These are needed to run the image # libblas3 libgomp1?
ENV BUILD_APT_DEPENDENCIES="wget xzdec texlive gpg r-base git-lfs"
RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}

# Finish up the configuration
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata

### Setup R packages
RUN echo "options(Ncpus = $(nproc --all))" >> /usr/local/lib/R/etc/Rprofile.site

# These come from CRAN
ENV BUILD_CRAN_DEPENDENCIES="reshape dplyr mgcv zipcode ggplot2 data.table splitstackshape maps knitr devtools tikzDevice rgdal"
RUN install2.r ${BUILD_CRAN_DEPENDENCIES}

# These come from GitHub
ENV BUILD_R_GITHUB_DEPENDENCIES="kassambara/easyGgplot2 airbornemint/outbreak-predict"
RUN Rscript -e "devtools::install_github(commandArgs(trailingOnly=TRUE))" ${BUILD_R_GITHUB_DEPENDENCIES}

### Setup TeX packages
RUN tlmgr init-usertree

ENV BUILD_TEX_DEPENDENCIES="framed"
RUN tlmgr install ${BUILD_TEX_DEPENDENCIES}

### Cleanup to reduce size of the image
RUN apt-get remove -y -qq ${DOCKER_APT_DEPENDENCIES}
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm -rf /tmp/downloaded_packages

ENV LC_CTYPE=ISO.UTF-8 LANG=ISO.UTF-8

ENTRYPOINT /bin/bash