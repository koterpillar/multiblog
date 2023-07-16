FROM ubuntu:22.04

ADD install-deps /
RUN /install-deps

ADD multiblog /bin

RUN mkdir /content
WORKDIR /content

ENTRYPOINT /bin/multiblog
