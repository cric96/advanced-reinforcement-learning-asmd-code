ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.3.3",
    name := "multi-agent-system-modelling",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "dev.scalapy" %% "scalapy-core" % "0.5.3",
      "org.scalactic" %% "scalactic" % "3.2.14",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    ),
  )

fork := true
import ai.kien.python.Python
lazy val python = Python()
lazy val javaOpts = python.scalapyProperties.get.map {
  case (k, v) => s"""-D$k=$v"""
}.toSeq
javaOptions ++= javaOpts
