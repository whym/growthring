// -*- mode: scala -*-

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.carrotsearch"   % "jsuffixarrays" % "0.1.0",
  "net.java.dev.jna"   % "jna"           % "5.6.0",
  "org.apache.commons" % "commons-text" % "1.9",
  "javax.servlet"  % "javax.servlet-api" % "3.1.0" % "provided",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.2",
  "org.json4s"        %% "json4s-native" % "3.6.10",
  "com.typesafe"       % "config"        % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback"     % "logback-classic" % "1.2.3",
  "org.scalatest"     %% "scalatest"     % "3.2.2" % "test",
  "org.scalatestplus" %% "scalatestplus-mockito" % "1.0.0-M2" % Test,
  "org.mockito"        % "mockito-core"  % "3.5.13" % "test"
)

Compile / run / mainClass := Some("org.whym.growthring.Main")

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

enablePlugins(TomcatPlugin)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    scalaVersion := "2.13.6",
    organization := "org.whym",
    name := "growthring",
    version := "0.6-SNAPSHOT",
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      // "-opt:l:inline",
      // "-opt-inline-from:**",
      // "-opt:box-unbox",
      // "-opt:redundant-casts",
      // "-opt:simplify-jumps",
      "-explaintypes",
      "-feature",
      "-g:line"),
    javacOptions ++= Seq(
      "-source", "1.8"
    ),
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
