import sbt._
import Keys._
import play.Project._
import java.io.File

object ApplicationBuild extends Build {

  val appName         = "growthring-play"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.whym" %% "growthring" % "0.3"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // For additionally resolving from the conventional ivy local home.
    resolvers += Resolver.file("LocalIvy", file(Path.userHome +
      File.separator + ".ivy2" + File.separator +
      "local"))(Resolver.ivyStylePatterns),

    templatesImport += "org.whym.growthring._"
  )

}
