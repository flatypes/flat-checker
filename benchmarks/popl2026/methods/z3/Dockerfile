# Use Ubuntu 22.04 as base (or ubuntu:20.04)
FROM ubuntu:22.04

# Install essentials
RUN apt-get update \
    && apt-get install -y git build-essential python3 \
    && rm -rf /var/lib/apt/lists/*

# Clone Z3 and checkout 4.15.1
RUN git clone https://github.com/Z3Prover/z3.git /opt/z3 \
    && cd /opt/z3 \
    && git checkout z3-4.15.1 \
    && python3 scripts/mk_make.py --prefix=/usr/local \
    && cd build \
    && make -j$(nproc) \
    && make install

# Clean up
RUN apt-get remove --purge -y git build-essential python3 \
    && apt-get autoremove -y \
    && rm -rf /opt/z3

ENTRYPOINT ["z3"]
CMD ["--help"]
