// -*- mode: scala -*-

organization := "org.whym"
name    := "growthring"
version := "0.5-SNAPSHOT"
scalaVersion := "2.11.6"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-explaintypes",
  "-feature",
  "-Xmax-classfile-name", "128",
  "-g:line")
javacOptions ++= Seq(
  "-source", "1.8"
)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "4.1.0",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "javax.servlet"  % "javax.servlet-api" % "3.1.0" % "provided",
  "org.json4s"        %% "json4s-native" % "3.2.9",
  "com.typesafe"       % "config"        % "1.2.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback"     % "logback-classic" % "1.1.7",
  "org.scalatest"     %% "scalatest"     % "2.1.3" % "test",
  "org.mockito"        % "mockito-core"  % "1.9.5" % "test",
  "junit"              % "junit"         % "4.11"  % "test"
)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")
mainClass in assembly := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

enablePlugins(TomcatPlugin)

buildInfoSettings
sourceGenerators in Compile <+= buildInfo
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoKeys += BuildInfoKey.action("buildTime") {
  java.time.Instant.now
}
buildInfoPackage := "org.whym.growthring"
