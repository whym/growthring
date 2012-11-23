// -*- mode: scala -*-
name    := "growthring"

version := "0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation",
                      "-unchecked")

libraryDependencies ++= Seq(
  // "org.apache.commons" % "commons-lang3" % "3.1",
  // "net.liftweb" % "lift-json_2.9.1" % "2.4",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "junit" % "junit" % "4.8.1" % "test"
)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")
