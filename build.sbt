name := "chess-simulation"

version := "0.1"

scalaVersion := "2.13.8"

unmanagedBase := baseDirectory.value / "libs"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0" % Test
)