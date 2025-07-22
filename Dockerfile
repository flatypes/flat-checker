FROM ubuntu:latest

# Switch from `sh -c` to `bash -c` as the shell behind a `RUN` command.
SHELL ["/bin/bash", "-c"]

# Install SDKMAN, and then install `java` and `sbt`.
RUN apt-get update
RUN apt-get -y install curl bash unzip zip
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

# Building CVC5 and its Java bindings from source.
RUN apt-get -y install git cmake m4 python3-venv python3-pip
WORKDIR /work
RUN git clone https://github.com/cvc5/cvc5
WORKDIR /work/cvc5
RUN ./configure.sh production --java-bindings --auto-download --prefix=build/install
WORKDIR /work/cvc5/build
RUN make -j8
RUN make install
# Copy the JNI shared library to `/usr/lib/` to make sure that JVM can find and load it.
RUN ln -s /work/cvc5/build/install/lib/libcvc5jni.so /usr/lib/

# Setting up our main working directory.
WORKDIR /work/flat-checker
COPY . .
# Copy the newly built `cvc5.jar`, as the old one provided in the project repo seems only work on my local laptop.
# This `jar` should be removed from the repo in future.
RUN cp /work/cvc5/build/install/share/java/cvc5.jar lib/cvc5-1.2.0.jar

# Finally, we can build our sbt project and test an example.
RUN sbt assembly
RUN ./flat-checker examples/a_star.py

ENTRYPOINT [ "bash" ]