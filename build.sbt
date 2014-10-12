// -*- mode: scala -*-
import AssemblyKeys._

organization := "org.whym"

name    := "growthring"

version := "0.4"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation",
                      "-unchecked",
                      "-optimise",
                      "-explaintypes",
                      "-feature",
                      "-Xmax-classfile-name", "128",
                      "-g:line")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "4.1.0",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "javax.servlet"      % "servlet-api"   % "2.5",
  "org.json4s"        %% "json4s-native" % "3.2.9",
  "com.typesafe"       % "config"        % "1.2.0",
  "org.slf4j"          % "slf4j-api"     % "1.7.7",
  "com.typesafe"      %% "scalalogging-slf4j" % "1.1.0",
  "ch.qos.logback"     % "logback-classic" % "1.1.2",
  "org.scalatest"     %% "scalatest"     % "2.1.3" % "test",
  "org.mockito"        % "mockito-core"  % "1.9.5" % "test",
  "junit"              % "junit"         % "4.11"  % "test"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

assemblySettings

mainClass in (Compile, run) := Some("org.whym.growthring.Main")

mainClass in oneJar := Some("org.whym.growthring.Main")

mainClass in assembly := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
