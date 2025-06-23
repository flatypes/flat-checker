FROM ubuntu:22.04

# Install runtime dependencies
RUN apt-get update && apt-get install -y libgmp-dev curl unzip && \
    rm -rf /var/lib/apt/lists/*

# Set version
ARG CVC5_VERSION=1.3.0
WORKDIR /usr/local/bin

# Download and install prebuilt binary
RUN curl -L -o cvc5.zip https://github.com/cvc5/cvc5/releases/download/cvc5-${CVC5_VERSION}/cvc5-Linux-x86_64-static.zip \
    && unzip cvc5.zip

ENV PATH="/usr/local/bin/cvc5-Linux-x86_64-static/bin:${PATH}"