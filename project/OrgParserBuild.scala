import sbt._
import sbt.Keys._

object OrgParserBuild extends Build {

  lazy val root = Project(
    id = "org-parser",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "org-parser",
      organization := "com.github.tototoshi",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      scalacOptions <<= scalaVersion.map { sv =>
        if (sv.startsWith("2.10")) {
          Seq(
            "-deprecation",
            "-language:_"
          )
        } else {
          Seq("-deprecation")
        }
      },
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "2.1.0",
        "commons-io" % "commons-io" % "2.4"
      )
    )
  )
}
