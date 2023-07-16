FROM ubuntu:22.04

RUN \
      apt update && \
      apt install --yes xvfb wget && \
      wget -O wkhtmltox.deb https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6-3/wkhtmltox_0.12.6-3.jammy_amd64.deb && \
      apt install --yes ./wkhtmltox.deb && \
      rm wkhtmltox.deb && \
      true

ADD multiblog /bin

RUN mkdir /content
WORKDIR /content

ENTRYPOINT /bin/multiblog
