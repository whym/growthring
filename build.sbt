// -*- mode: scala -*-

resolvers ++= Resolver.sonatypeOssRepos("releases")

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "5.6.0",
  "org.apache.commons" % "commons-text" % "1.9",
  "javax.servlet"  % "javax.servlet-api" % "3.1.0" % "provided",
  "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
  ("org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.2").cross(CrossVersion.for3Use2_13),
  "org.json4s"        %% "json4s-native" % "4.0.4",
  "com.typesafe"       % "config"        % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "ch.qos.logback"     % "logback-classic" % "1.2.11",
  "com.softwaremill.sttp.client3" %% "core" % "3.9.2" % "test",
  "org.scalatest"     %% "scalatest"     % "3.2.10" % "test",
  "org.scalatestplus" %% "mockito-3-4" % "3.2.10.0" % "test",
)

Compile / run / mainClass := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

enablePlugins(TomcatPlugin)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    scalaVersion := "3.3.0",
    organization := "org.whym",
    name := "growthring",
    version := "0.7-SNAPSHOT",
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      // "-opt:l:inline",
      // "-opt-inline-from:**",
      // "-opt:box-unbox",
      // "-opt:redundant-casts",
      // "-opt:simplify-jumps",
      "-explaintypes",
      "-feature"),
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
  .setPreference(DoubleIndentConstructorArguments, true)
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
