ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.3"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-Wsafe-init",
  "-source", "3.7"
)

lazy val root = (project in file("."))
  .settings(
    name := "flat-checker"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.apache.commons" % "commons-text" % "1.13.0"
libraryDependencies += "org.apache.commons" % "commons-math" % "2.2"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.4"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "com.ibm.icu" % "icu4j" % "78.1"
libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0"
libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j.jsonrpc" % "1.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

enablePlugins(Antlr4Plugin)
Antlr4 / antlr4Version := "4.13.0"
Antlr4 / antlr4PackageName := Some("flat.antlr")
Antlr4 / antlr4GenListener := false
Antlr4 / antlr4GenVisitor := true

assembly / assemblyOutputPath := file("target/flat-checker.jar")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs@_*) =>
    xs.map(_.toLowerCase) match {
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.discard
    }
  case _ => MergeStrategy.first
}