organization := "asci"

name := "asci"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalamock" % "scalamock-core_2.10" % "3.1.RC1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.1.4" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.11.3" % "test"
)
