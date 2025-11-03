FROM python:3.12-slim

# Switch from `sh -c` to `bash -c` as the shell behind a `RUN` command.
SHELL ["/bin/bash", "-c"]

# Install SDKMAN, and then install `java` and `sbt`.
RUN apt-get update
RUN apt-get -y install curl bash unzip zip jq
RUN curl -s "https://get.sdkman.io" | bash
# FUN FACTS:
# 1) the `sdk` command is not a binary but a bash script loaded into memory
# 2) Shell sessions are a "process", which means environment variables
#    and declared shell function only exist for
#    the duration that shell session exists
RUN source "$HOME/.sdkman/bin/sdkman-init.sh" \
    && sdk install java 21.0.2-open \
    && sdk install sbt 1.10.0
# Once the real binaries exist these are the symlinked paths that need to exist on PATH.
ENV PATH=/root/.sdkman/candidates/java/current/bin:$PATH
ENV PATH=/root/.sdkman/candidates/sbt/current/bin:$PATH

# Setting up our main working directory.
WORKDIR /work/flat-checker
COPY . .
RUN python3 scripts/install_cvc5.py

# Finally, we build and test FLAT-Checker.
RUN sbt test
RUN sbt assembly
RUN ./flat-checker examples/a_star.py

ENTRYPOINT [ "bash" ]