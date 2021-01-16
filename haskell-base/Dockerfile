## Dockerfile for a haskell environment
## Adapted from https://raw.githubusercontent.com/freebroccolo/docker-haskell/master/8.2/Dockerfile
FROM       debian:buster

## ensure locale is set during build
ENV LANG            C.UTF-8
ENV DEBIAN_FRONTEND noninteractive

# from https://github.com/inversepath/usbarmory-debian-base_image/issues/9
RUN mkdir ~/.gnupg && echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg ca-certificates dirmngr curl git
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ git curl netbase build-essential make libgmp3-dev

RUN curl -fSL https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64.tar.gz -o stack.tar.gz && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v2.5.1/stack-2.5.1-linux-x86_64.tar.gz.asc -o stack.tar.gz.asc

# generated with:
# gpg --keyserver ha.pool.sks-keyservers.net --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442 && \
# gpg --export -a C5705533DA4F78D8664B5DC0575159689BEFB442 > haskell.key
COPY haskell.key /.
RUN export GNUPGHOME="$(mktemp -d)" && \
    gpg --import /haskell.key && \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz

RUN tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 && \
    rm -rf "$GNUPGHOME" /var/lib/apt/lists/* /stack.tar.gz.asc /stack.tar.gz /haskell.key

## Create special user 'user' and prepare its environment
RUN useradd --shell /bin/bash -u 500 -o -c "" -m user
USER user

ARG STACKAGE_VERSION=lts-16.18

RUN /usr/local/bin/stack setup --resolver $STACKAGE_VERSION

VOLUME /home/user/.stack

USER root

# assumes entry.sh is in the context
COPY entry.sh /usr/local/bin/entry.sh

ENTRYPOINT ["/usr/local/bin/entry.sh"]
