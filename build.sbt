organization := "asci"

name := "asci"

scalaVersion := "2.11.0"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalamock" % "scalamock-core_2.11" % "3.1.1" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.1.4" % "test",
  "org.scalacheck" % "scalacheck_2.11" % "1.11.3" % "test",
  "org.scalaz" % "scalaz-core_2.11" % "7.0.6"
)
