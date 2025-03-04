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
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.4"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

assembly / assemblyOutputPath := file("target/flat-checker.jar")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}