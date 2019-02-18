ARG BUILDER_BASE

FROM $BUILDER_BASE
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages
RUN apt-get update -y -qq
RUN apt-get install -y -qq --no-install-recommends gosu

### Cleanup to reduce size of the image
RUN apt-get autoremove -y
RUN apt-get clean

ENV LC_CTYPE=ISO.UTF-8 LANG=ISO.UTF-8

### Set up a gosu/useradd entrypoint
COPY ./entrypoint /usr/local/bin/entrypoint
RUN chmod a+x /usr/local/bin/entrypoint

ENTRYPOINT ["/usr/local/bin/entrypoint"]
