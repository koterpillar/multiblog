FROM ubuntu:focal

RUN \
      apt update && \
      apt install --yes xvfb wget && \
      wget -O wkhtmltox.deb https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6-1/wkhtmltox_0.12.6-1.focal_amd64.deb && \
      apt install --yes ./wkhtmltox.deb && \
      true

# stack --no-terminal build --copy-bins --local-bin-path .
ADD multiblog .

ENTRYPOINT multiblog
