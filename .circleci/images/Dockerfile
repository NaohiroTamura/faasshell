#
# JPL enabled SWI Prolog 8.0.3
# https://github.com/NaohiroTamura/docker-swipl/tree/swi-8.0.3-jpl
#
# $ docker build -t nao16t/swipl8jpl .
#
FROM debian:stretch-slim
LABEL maintainer "Dave Curylo <dave@curylo.org>, Michael Hendricks <michael@ndrix.org>"
RUN mkdir -p /usr/share/man/man1 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    libarchive13 \
    libgmp10 \
    libossp-uuid16 \
    libssl1.1 \
    ca-certificates \
    libdb5.3 \
    libpcre3 \
    libedit2 \
    libgeos-c1v5 \
    libspatialindex4v5 \
    unixodbc \
    odbc-postgresql \
    tdsodbc \
    libmariadbclient18 \
    libsqlite3-0 \
    libserd-0-0 \
    libraptor2-0 \
    openjdk-8-jdk junit zip unzip curl wget binutils git openssh-client make procps \
    iputils-ping iproute2 traceroute dnsutils tcpdump kafkacat socat net-tools vim-tiny gettext-base expect && \
    rm -rf /usr/share/man/man1 && \
    dpkgArch="$(dpkg --print-architecture)" && \
    { [ "$dpkgArch" = 'armhf' ] || [ "$dpkgArch" = 'armel' ] || apt-get install -y --no-install-recommends librocksdb4.5; } && \
    rm -rf /var/lib/apt/lists/*
RUN set -eux; \
    SWIPL_VER=8.0.3; \
    SWIPL_CHECKSUM=cee59c0a477c8166d722703f6e52f962028f3ac43a5f41240ecb45dbdbe2d6ae; \
    BUILD_DEPS='cmake gcc g++ autoconf libarchive-dev libgmp-dev libossp-uuid-dev libpcre3-dev libreadline-dev libedit-dev libssl-dev zlib1g-dev libdb-dev unixodbc-dev libsqlite3-dev libserd-dev libraptor2-dev libgeos++-dev libspatialindex-dev'; \
    dpkgArch="$(dpkg --print-architecture)"; \
    [ "$dpkgArch" = 'armhf' ] || [ "$dpkgArch" = 'armel' ] || BUILD_DEPS="$BUILD_DEPS librocksdb-dev"; \
    apt-get update; apt-get install -y --no-install-recommends $BUILD_DEPS; rm -rf /var/lib/apt/lists/*; \
    mkdir /tmp/src; \
    cd /tmp/src; \
    wget http://www.swi-prolog.org/download/stable/src/swipl-$SWIPL_VER.tar.gz; \
    echo "$SWIPL_CHECKSUM  swipl-$SWIPL_VER.tar.gz" >> swipl-$SWIPL_VER.tar.gz-CHECKSUM; \
    sha256sum -c swipl-$SWIPL_VER.tar.gz-CHECKSUM; \
    tar -xzf swipl-$SWIPL_VER.tar.gz; \
    mkdir swipl-$SWIPL_VER/build; \
    cd swipl-$SWIPL_VER/build; \
    cmake -DCMAKE_BUILD_TYPE=Release \
          -DSWIPL_PACKAGES_X=OFF \
	  -DCMAKE_INSTALL_PREFIX=/usr \
          ..; \
    # LANG=C.UTF8 is a work-around for a 7.7.22 bug
    LANG=C.UTF8 make; \
    LANG=C.UTF8 make install; \
    rm -rf /tmp/src; \
    mkdir -p /usr/lib/swipl/pack; \
    cd /usr/lib/swipl/pack; \
    # usage: install_addin addin-name git-url git-commit
    install_addin () { \
        git clone "$2" "$1"; \
        git -C "$1" checkout -q "$3"; \
        # the prosqlite plugin lib directory must be removed?
        if [ "$1" = 'prosqlite' ]; then rm -rf "$1/lib"; fi; \
        swipl -g "pack_rebuild($1)" -t halt; \
        find "$1" -mindepth 1 -maxdepth 1 ! -name lib ! -name prolog ! -name pack.pl -exec rm -rf {} +; \
        find "$1" -name .git -exec rm -rf {} +; \
    }; \
    dpkgArch="$(dpkg --print-architecture)"; \
    install_addin space https://github.com/JanWielemaker/space.git cd6fefa63317a7a6effb61a1c5aee634ebe2ca05; \
    install_addin prosqlite https://github.com/nicos-angelopoulos/prosqlite.git 816cb2e45a5fb53290a763a3306e430b72c40794; \
    [ "$dpkgArch" = 'armhf' ] || [ "$dpkgArch" = 'armel' ] || install_addin rocksdb https://github.com/JanWielemaker/rocksdb.git 93f29d8f298d73de5719b93516acc73e00610eed; \
    [ "$dpkgArch" = 'armhf' ] || [ "$dpkgArch" = 'armel' ] ||  install_addin hdt https://github.com/JanWielemaker/hdt.git e0a0eff87fc3318434cb493690c570e1255ed30e; \
    install_addin rserve_client https://github.com/JanWielemaker/rserve_client.git befdfab843d71bdabbe9574348697a6fe5be4fc3; \
    apt-get purge -y --auto-remove $BUILD_DEPS
RUN curl -sL https://download.docker.com/linux/static/stable/x86_64/docker-19.03.5.tgz | tar zxvf - --strip 1 -C /usr/local/bin docker/docker
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl && chmod 0755 kubectl && mv kubectl /usr/local/bin
RUN curl -L https://github.com/openshift/source-to-image/releases/download/v1.2.0/source-to-image-v1.2.0-2a579ecd-linux-amd64.tar.gz | tar zxvf - -C /usr/local/bin ./s2i
ENV LANG C.UTF-8
ENV LD_LIBRARY_PATH /usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64/server:/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64
COPY http_ssl_plugin.pl /usr/lib/swipl/library/http/http_ssl_plugin.pl
CMD ["swipl"]
