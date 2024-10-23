FROM haskell:9.6.6
WORKDIR /opt/build

# Install necessary dependencies for building
RUN apt-get update && apt-get install -y \
    liblzma-dev \
    pkg-config \
    zlib1g-dev \
    libpq-dev

COPY . .
RUN stack build && stack install

EXPOSE 8080

CMD ["/usr/local/bin/stack", "run"]

