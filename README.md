# FLAT Checker

Type checking regular language types.

## Building via Docker

Building the image:

```shell
docker build . -t flat-checker:dev
```

Running the image:

```shell
docker run -it flat-checker:dev
```

This will drop you into a bash shell inside the container.
You can run the type checker using the command `./flat-checker` as described below in the Usage section.

## Building from Source

Prerequisites:

- Java version 21 or later
- [sbt](https://www.scala-sbt.org) version 1.10.0
- Python version 3.12 or later
- [cvc5](https://cvc5.github.io) version 1.3.1 with Java bindings

For the first two prerequisites, you may install them through [sdkman](https://sdkman.io):

```shell
sdk install java 21.0.2-open
sdk install sbt 1.10.0
```

We use cvc5 as our backend SMT solver and invoke it through its Java bindings.
Starting from version 1.2.1, a self-contained JAR file is [available](https://github.com/cvc5/cvc5/releases/tag/cvc5-1.3.1).
Run the following Python script to download this platform-specific JAR (available for 64-bit x86/arm Linux/macOS): 

```shell
python3 scripts/install_cvc5.py
```

In case your platform is not supported, check the official building [instructions](https://cvc5.github.io/docs/cvc5-1.3.1/api/java/java.html).

Now, we are ready to build FLAT-Checker from source. In the project root:

- Run `sbt compile` to compile the source code.
- Run `sbt test` to run all unit tests.
- Run `sbt assembly` to build a standalone JAR.
- Run `./flat-checker examples/a_star.py` to type-check an example.

## Usage

Type `./flat-checker -h` to see the usage:

```text
Usage: flat-checker [options] <file>...

  <file>...                input files/directories
  --no-error               ensure no type errors (if not, exit on first error)
  --metrics <file>         collect and save statistical metrics to a JSON file
  --smt-time-limit <time>  time limit per SMT query in ms (default 3000)
  -h, --help               print this usage text
```

Each input file can be either a Python file or a directory.
For the latter, FLAT-Checker will recursively search for all Python files in the directory (and every subdirectory) and
type-check them.

By default, logs are output to stdout and a file `scala-logging.log`.
To change the log level as well as destinations, modify the logback configuration file `src/main/resources/logback.xml`
and rebuild the project via `sbt assembly`.

To just extract the SMT queries as SMT-LIB files without performing type checking, type `./flat-checker extract -h` to
see the usage.

¬