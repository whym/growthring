// -*- mode: scala -*-

organization := "org.whym"
name    := "growthring"
version := "0.5-SNAPSHOT"
scalaVersion := "2.12.9"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-opt:l:inline",
  "-opt-inline-from:**",
  "-opt:box-unbox",
  "-opt:redundant-casts",
  "-opt:simplify-jumps",
  "-explaintypes",
  "-feature",
  "-Xmax-classfile-name", "128",
  "-g:line")
javacOptions ++= Seq(
  "-source", "1.8"
)

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "4.1.0",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "javax.servlet"  % "javax.servlet-api" % "3.1.0" % "provided",
  "org.json4s"        %% "json4s-native" % "3.5.5",
  "com.typesafe"       % "config"        % "1.2.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0",
  "ch.qos.logback"     % "logback-classic" % "1.2.3",
  "org.scalatest"     %% "scalatest"     % "3.0.8" % "test",
  "org.mockito"        % "mockito-core"  % "2.28.2" % "test"
)

mainClass in (Compile, run) := Some("org.whym.growthring.Main")
mainClass in assembly := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

enablePlugins(TomcatPlugin)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, BuildInfoKey.action("buildTime") {
      java.time.Instant.now()
    }),
    buildInfoPackage := "org.whym.growthring",
    buildInfoOptions += BuildInfoOption.ToMap
  )

import scalariform.formatter.preferences._

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 40)
  .setPreference(CompactControlReadability, false)
  .setPreference(CompactStringConcatenation, false)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(FormatXml, true)
  .setPreference(IndentLocalDefs, false)
  .setPreference(IndentPackageBlocks, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(IndentWithTabs, false)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)
  .setPreference(DanglingCloseParenthesis, Preserve)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(PreserveSpaceBeforeArguments, false)
  .setPreference(RewriteArrowSymbols, false)
  .setPreference(SpaceBeforeColon, false)
  .setPreference(SpaceInsideBrackets, false)
  .setPreference(SpaceInsideParentheses, false)
  .setPreference(SpacesWithinPatternBinders, true)
