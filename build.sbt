ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.3.3",
    name := "multi-agent-system-modelling",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "com.github.haifengl" %% "smile-scala" % "6.0.1",
      "com.github.haifengl"  % "smile-deep"  % "6.0.1",
      "org.scalactic" %% "scalactic" % "3.2.14",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    ),
  )

fork := true
javaHome := Some(file("/usr/lib/jvm/java-25-openjdk"))
javaOptions ++= Seq(
  "-Xmx4G",
  "-Djava.library.path=/usr/lib/x86_64-linux-gnu",
  "--enable-native-access=ALL-UNNAMED"
)
