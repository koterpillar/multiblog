FROM ubuntu:24.04

ADD install-deps /
RUN /install-deps

ADD multiblog /bin
RUN chmod +x /bin/multiblog

RUN mkdir /content
WORKDIR /content

ENTRYPOINT /bin/multiblog
