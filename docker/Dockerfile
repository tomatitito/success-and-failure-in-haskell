FROM ubuntu:18.04
RUN mkdir -p /opt/app
#ARG BINARY_PATH
#COPY $BINARY_PATH /opt/app
COPY .stack-work/install/x86_64-linux/ba22c0cc6bb133369c9553e4891f9dfedb74aebbbf599cd46e6727e96287990e/8.6.5/bin /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
ENTRYPOINT /opt/app/validating-book
