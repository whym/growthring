// -*- mode: scala -*-
organization := "org.whym"

name    := "growthring"

version := "0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation",
                      "-unchecked",
                      "-optimise",
                      "-explaintypes",
                      "-g:source")

libraryDependencies ++= Seq(
  "com.carrotsearch" % "jsuffixarrays" % "0.1.0",
  //"it.unimi.dsi" % "sux4j" % "3.0.5",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "junit" % "junit" % "4.8.1" % "test"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")

mainClass in oneJar := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
