val dottyVersion = "3.0.0-M2"
val scala213Version = "2.13.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lambda",
    version := "1.0.0",
    scalaVersion := dottyVersion,
    crossScalaVersions := Seq(dottyVersion, scala213Version),
    useScala3doc := true,
  )
