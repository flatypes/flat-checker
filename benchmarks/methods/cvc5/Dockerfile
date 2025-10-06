FROM python:3.12-slim

RUN apt-get update
RUN apt-get install -y git build-essential cmake m4

# Build and install cvc5 1.3.1
RUN git clone https://github.com/cvc5/cvc5 /opt/cvc5 \
    && cd /opt/cvc5 \
    && git checkout cvc5-1.3.1 \
    && ./configure.sh production --auto-download \
    && cd build \
    && make -j$(nproc) \
    && make install

ENTRYPOINT ["cvc5"]
CMD ["--version"]