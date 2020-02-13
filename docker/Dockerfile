FROM ubuntu:18.04
RUN mkdir -p /opt/app
ARG BINARY_PATH
COPY $BINARY_PATH /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
ENTRYPOINT /opt/app/validating-book
