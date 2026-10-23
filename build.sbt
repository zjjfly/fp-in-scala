ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.18"

val mainClassName = "com.github.zjjfly.Main"

lazy val root = (project in file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "fp-in-scala",
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.23.0" % "test",
      "org.specs2" %% "specs2-mock" % "4.23.0" % "test"
    ),
    idePackagePrefix := Some("com.github.zjjfly.fp"),
    assembly / mainClass := Some(mainClassName),
    Compile / mainClass := Some(mainClassName),
    //make sure GRAAL_HOME or GRAALVM_HOME in env
    nativeImageInstalled := true
  )
