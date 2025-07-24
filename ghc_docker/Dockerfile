ARG GHC_VERSION
FROM debian:bookworm
ARG GHC_VERSION

LABEL ghc.version="${GHC_VERSION}"

RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        build-essential \
        curl \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        zlib1g-dev \
        xz-utils \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/ghc-install

RUN set -ex && \
    # Wähle das passende Debian-Release für die GHC-Binary-URL
    case "${GHC_VERSION}" in \
      8.*|8.10.*|8.8.*|8.6.*|8.4.*|8.2.*|8.0.*) DEB_REL="deb10" ;; \
      9.0.*|9.2.*) DEB_REL="deb10" ;; \
      *) DEB_REL="deb11" ;; \
    esac && \
    GHC_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-${DEB_REL}-linux.tar.xz" && \
    curl -L --retry 3 "${GHC_URL}" -o ghc.tar.xz && \
    tar -xf ghc.tar.xz && \
    cd ghc-*/ && \
    ./configure && \
    make install && \
    ghc --version && \
    rm -rf /tmp/ghc-install

ENV LC_ALL="C.UTF-8"
RUN chmod 1777 /home && \
    adduser --uid 996 --home /home/tester --no-create-home \
            --shell /bin/bash --disabled-password --system --group tester

USER tester
WORKDIR /home/tester
CMD ["ghc", "--version"]