name := "scanet2"

version := "0.1"

scalaVersion := "2.12.8"


libraryDependencies ++= Seq(
  // "org.typelevel" %% "spire" % "0.14.1",
  "org.bytedeco" % "tensorflow-platform" % "1.15.0-1.5.2",
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
  )