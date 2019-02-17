FROM rocker/r-ver
LABEL maintainer="Ben Artin <ben@artins.org>"

ENV BUILD_APT_DEPENDENCIES="libcurl4-openssl-dev zlib1g-dev libssl-dev texlive wget xzdec libgdal-dev"
ENV BUILD_CRAN_DEPENDENCIES="reshape dplyr mgcv zipcode ggplot2 data.table splitstackshape maps knitr devtools tikzDevice rgdal"
ENV BUILD_R_GITHUB_DEPENDENCIES="kassambara/easyGgplot2 airbornemint/outbreak-predict"
ENV BUILD_TEX_DEPENDENCIES=""

RUN apt-get update -y -qq
RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}
RUN echo "options(Ncpus = $(nproc --all))" >> /usr/local/lib/R/etc/Rprofile.site
RUN install2.r ${BUILD_CRAN_DEPENDENCIES}
RUN Rscript -e "devtools::install_github(commandArgs(trailingOnly=TRUE))" ${BUILD_R_GITHUB_DEPENDENCIES}
# RUN tlmgr init-usertree
# RUN tlmgr install xxx
RUN /usr/bin/apt-get autoremove -y
RUN /usr/bin/apt-get clean

ENV LC_CTYPE=ISO.UTF-8 LANG=ISO.UTF-8

ENTRYPOINT "/bin/bash"
