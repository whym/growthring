// -*- mode: scala -*-
name    := "growthring"

version := "0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation",
                      "-unchecked",
                      //"-optimise",
                      "-g:source")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "junit" % "junit" % "4.8.1" % "test"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")

mainClass in oneJar := Some("org.whym.growthring.Main")
