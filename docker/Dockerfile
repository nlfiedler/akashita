#
# Use ubuntu as a base in order to easily get ZFS support.
# It is trivial to add a recent Erlang/OTP as well.
#
# This container requires a host with the zfs kernel module.
# i.e. docker-machine will not suffice, but an Ubuntu VM will.
#
FROM ubuntu:18.04

#
# We need rebar3, which itself requires Erlang/OTP and git.
#
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get -q -y install \
    apt-utils \
    erlang \
    git \
    zfsutils-linux
ADD https://github.com/erlang/rebar3/releases/download/3.7.4/rebar3 /bin/rebar3
RUN chmod +x /bin/rebar3

WORKDIR /work

COPY config config
COPY rebar.config .
COPY rebar.config.script .
COPY src src
COPY test test
RUN rebar3 release

ENTRYPOINT [ "/bin/bash" ]
