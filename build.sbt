// -*- mode: scala -*-

organization := "org.whym"
name    := "growthring"
version := "0.6-SNAPSHOT"
scalaVersion := "2.12.12"
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

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "5.6.0",
  "org.apache.commons" % "commons-text" % "1.9",
  "javax.servlet"  % "javax.servlet-api" % "3.1.0" % "provided",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.json4s"        %% "json4s-native" % "3.6.0",
  "com.typesafe"       % "config"        % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback"     % "logback-classic" % "1.2.3",
  "org.scalatest"     %% "scalatest"     % "3.2.2" % "test",
  "org.scalatestplus" %% "scalatestplus-mockito" % "1.0.0-M2" % Test,
  "org.mockito"        % "mockito-core"  % "3.5.13" % "test"
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
