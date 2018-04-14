#
# JPL enabled SWI Prolog 7.5.15
# https://github.com/NaohiroTamura/docker-swipl/tree/swi-7.5.15-jpl
#
# $ docker build -t nao16t/swipl7jpl .
#
FROM debian:stretch-slim
LABEL maintainer "Dave Curylo <dave@curylo.org>, Michael Hendricks <michael@ndrix.org>"
RUN mkdir -p /usr/share/man/man1
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libarchive13 \
    libgmp10 \
    libossp-uuid16 \
    libssl1.1 \
    libdb5.3 \
    libpcre3 \
    libedit2 \
    libgeos-c1v5 \
    libspatialindex4v5 \
    unixodbc \
    odbc-postgresql \
    tdsodbc \
    libmariadbclient18 \
    openjdk-8-jdk junit zip unzip ca-certificates curl binutils git openssh-client make procps && \
    rm -rf /var/lib/apt/lists/*
RUN SWIPL_VER=7.5.15 && \
    SWIPL_CHECKSUM=aca07ce9c564e608586e7ae7b9a56c82ca5dd919cde1a1edf1121efa16bda568 && \
    BUILD_DEPS='gcc wget libarchive-dev libgmp-dev libossp-uuid-dev libpcre3-dev libreadline-dev libedit-dev libssl-dev zlib1g-dev libdb-dev libgeos-dev libspatialindex-dev unixodbc-dev' && \
    apt-get update && apt-get install -y --no-install-recommends $BUILD_DEPS && \
    mkdir /tmp/src && \
    cd /tmp/src && \
    wget http://www.swi-prolog.org/download/devel/src/swipl-$SWIPL_VER.tar.gz && \
    echo "$SWIPL_CHECKSUM  swipl-$SWIPL_VER.tar.gz" >> swipl-$SWIPL_VER.tar.gz-CHECKSUM && \
    sha256sum -c swipl-$SWIPL_VER.tar.gz-CHECKSUM && \
    tar -xzf swipl-$SWIPL_VER.tar.gz && \
    cd swipl-$SWIPL_VER && \
    cp build.templ build && \
    sed -i '/PREFIX=$HOME/c\PREFIX=/swipl' build && \
    sed -i '/# export DISABLE_PKGS/c\export DISABLE_PKGS="xpce"' build && \
    sed -i '/# export EXTRA_PKGS/c\export EXTRA_PKGS="db space"' build && \
    chmod u+x build && ./build && \
    apt-get purge -y --auto-remove $BUILD_DEPS && \
    cd /usr/bin && rm -rf /tmp/src && ln -s /swipl/bin/swipl swipl && rm -rf /var/lib/apt/lists/*
ENV LANG C.UTF-8
ENV LD_LIBRARY_PATH /usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64/server:/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64
RUN curl -L https://download.docker.com/linux/static/stable/x86_64/docker-17.12.1-ce.tgz | tar zxvf - --strip 1 -C /usr/local/bin docker/docker
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl && chmod 0755 kubectl && mv kubectl /usr/local/bin
RUN curl -L https://mirror.openshift.com/pub/openshift-v3/clients/3.9.14/linux/oc.tar.gz | tar zxvf - -C /usr/local/bin
RUN curl -L https://github.com/openshift/source-to-image/releases/download/v1.1.9a/source-to-image-v1.1.9a-40ad911d-linux-amd64.tar.gz | tar zxvf - -C /usr/local/bin ./s2i
RUN apt-get update && apt-get install -y iputils-ping iproute2 traceroute dnsutils tcpdump kafkacat socat net-tools vim-tiny gettext-base expect
COPY http_ssl_plugin.pl /swipl/lib/swipl-7.5.15/library/http/http_ssl_plugin.pl
CMD ["swipl"]
