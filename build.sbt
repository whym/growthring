// -*- mode: scala -*-
organization := "org.whym"

name    := "growthring"

version := "0.3"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation",
                      "-unchecked",
                      "-optimise",
                      "-explaintypes",
                      "-g:line")

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "3.2.2",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "javax.servlet"      % "servlet-api"   % "2.5",
  "org.json4s"        %% "json4s-native" % "3.2.4",
  "org.scalatest"     %% "scalatest"     % "2.0.M5b" % "test",
  "com.typesafe"      %% "scalalogging-slf4j" % "1.0.1",
  "com.typesafe"       % "config" % "1.0.0",
  "org.slf4j"          % "slf4j-api"     % "1.7.1",
  "ch.qos.logback"     % "logback-classic" % "1.0.7",
  "org.mockito"        % "mockito-core"  % "1.9.0" % "test",
  "junit"              % "junit"         % "4.8.1" % "test"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")

mainClass in oneJar := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
