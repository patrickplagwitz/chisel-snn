// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "%ORGANIZATION%"

Test / logBuffered := false
Test / parallelExecution := false

val chiselVersion = "5.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "snn-accel",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      //"edu.berkeley.cs" %% "chiseltest" % "5.0.2" % "test",
      "edu.berkeley.cs" %% "chiseltest" % "5.0.2",
      //"edu.berkeley.cs" %% "firrtl" % "5.0.0",
      "com.lihaoyi" %% "ujson" % "0.9.6"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
  )

