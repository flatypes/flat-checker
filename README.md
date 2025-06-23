# FLAT Checker

Type checking regular language types.

## Build (Via Docker)

Simply run `docker build . -t flat-checker:dev` to build a Docker image.
If succeeds, launch it by `docker run -it flat-checker:dev`.

## Build (From Source)

For building this Scala project, you need [sbt](https://www.scala-sbt.org) (in this project: version `1.10.0`).

We use [CVC5](https://cvc5.github.io) as a backend SMT solver to discharge queries about integer arithmetic.
We call it using the official Java-bindings. A prebuilt JAR dependency (version `1.2.0`) is included in the `lib/`
folder.
But you need to compile a shared library on your local machine and if necessary, add it to your Java library path so
that JVM can correctly load it.
For example, on macOS (M1 and later), you may need to copy `libcvc5jni.dylib` to `~/Library/Java/Extensions/lib/`.
Also, you should copy the `cvc5.jar` from the CVC5 folder into the `lib/` folder of this project.
See [here](https://cvc5.github.io/docs/cvc5-1.2.0/api/java/java.html) for more details on building the Java bindings.

In the project root folder:

- Run `sbt test` to compile and run all unit tests.
- Run `sbt assembly` to build a standalone jar. Then you can run it using the command `./flat-checker`.

## Usage

Type `./flat-checker -h` to see the usage.

For example, to type check `examples/a_star.py`, simply run

```shell
./flat-checker examples/a_star.py 
```

By default, logs will be directly output to stdout.
You may modify `src/main/resources/logback.xml` to change the logging settings.
The input file could also be a folder, in which FLAT-Checker will type check all Python files found in that folder.

To just extract the SMT queries without actually running any SMT solver, use the `--extract-only <folder>` option and
specify an output folder for the SMT-lib2 files:

```shell
./flat-checker --extract-only smt/ examples/panini-bench
```

This command will extract all generated SMT queries and output them into the `smt/` folder.
It will create a file for each query of each input Python source, in the path `smt/<SOURCE_NAME>/<QUERY_ID>.smt2`.
In compatibility consideration, we only extract the SMT assertions and assume each query may use `ALL` SMT logic.
No options are specified as they can be different from solvers to solvers; add them through command line if needed.