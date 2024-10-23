FROM haskell:9.6.6 AS build
WORKDIR /opt/build

# Install necessary dependencies for building
RUN apt-get update && apt-get install -y \
    liblzma-dev \
    pkg-config \
    zlib1g-dev \
    libpq-dev

COPY . .
RUN stack build && stack install

FROM haskell:9.6.6
WORKDIR /opt/app

# Copy the built binary from the build stage
COPY --from=build /root/.local/bin/ .

EXPOSE 8080

CMD ["./correct-me"]


