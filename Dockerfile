FROM haskell:9.6.6

WORKDIR /app

# Install necessary dependencies for building
RUN apt-get update && apt-get install -y \
    liblzma-dev \
    pkg-config \
    zlib1g-dev \
    libpq-dev

# Copy only the stack configuration files first to leverage caching
COPY stack.yaml stack.yaml
COPY package.yaml package.yaml

# Create dummy source directories to satisfy stack setup and build
RUN mkdir -p src exe test

# Prebuild standard libraries so that Docker can cache them
RUN stack build --only-dependencies

# Copy the rest of the application code
COPY . .

# Build and install the application
RUN stack build && stack install

# Prebuild tests
RUN stack build --test --no-run-tests

EXPOSE 8080

ENTRYPOINT ["/root/.local/bin/correct-me"]
