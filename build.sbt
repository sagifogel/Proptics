import BuildHelper._
import Dependencies._
import MimaSettings.mimaSettings
import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "io.github.sagifogel",
    homepage := Some(url("https://github.com/sagifogel/Proptics")),
    licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        "sagifogel",
        "Sagi Fogel",
        "sagi.fogel@gmail.com",
        url("https://github.com/sagifogel")
      )
    ),
    latestVersion := {
      val snapshot = (ThisBuild / isSnapshot).value
      val stable = (ThisBuild / isVersionStable).value

      if (!snapshot && stable) {
        (ThisBuild / version).value
      } else {
        (ThisBuild / previousStableVersion).value.getOrElse("0.0.0")
      }
    }
  ))

addCommandAlias("build", "prepare; testJVM")
addCommandAlias("prepare", "fix; fmt")
addCommandAlias("check", "fmtCheck; fixCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
addCommandAlias("fmt", "all proptics/scalafmtSbt proptics/scalafmtAll")
addCommandAlias("fixCheck", "compile:scalafix --check ; test:scalafix --check")
addCommandAlias("fmtCheck", "all proptics/scalafmtSbtCheck proptics/scalafmtCheckAll")
addCommandAlias("compileJVM", "propticsJVM/test:compile")
addCommandAlias("compileJS", "propticsJS/test:compile")
addCommandAlias("testJVM", "propticsJVM/test;")
addCommandAlias("testJS", "propticsJS/test;")
addCommandAlias(
  "mimaCheck",
  "all coreJVM/mimaReportBinaryIssues profunctorJVM/mimaReportBinaryIssues"
)
lazy val proptics = project
  .in(file("."))
  .settings(moduleName := "proptics")
  .settings(noPublishSettings)
  .settings(stdSettings)
  .settings(welcomeMessage)
  .aggregate(propticsJVM, propticsJS)
  .dependsOn(propticsJVM, propticsJS)
  .enablePlugins(ScalaJSPlugin)

lazy val propticsJVM = project
  .in(file(".propticsJVM"))
  .settings(noPublishSettings)
  .aggregate(core.jvm, profunctor.jvm, law.jvm, test.jvm, example)
  .dependsOn(core.jvm, profunctor.jvm, law.jvm, test.jvm)

lazy val propticsJS = project
  .in(file(".propticsJS"))
  .settings(noPublishSettings)
  .aggregate(core.js, profunctor.js, law.js, test.js)
  .dependsOn(core.js, profunctor.js, law.js, test.js)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .dependsOn(profunctor)
  .settings(stdProjectSettings("core"))
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))

lazy val coreJS = core.js
lazy val coreJVM = core.jvm.settings(mimaSettings(false))

lazy val profunctor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(stdProjectSettings("profunctor"))
  .settings(libraryDependencies ++= Seq(cats.value))

lazy val profunctorJS = profunctor.js
lazy val profunctorJVM = profunctor.jvm.settings(mimaSettings(false))

lazy val example = project
  .dependsOn(core.jvm, profunctor.jvm, test.jvm % "test->test")
  .settings(noPublishSettings)
  .settings(stdProjectSettings("example"))
  .settings(libraryDependencies ++= Seq(cats.value, catsLaws.value, spire.value, discipline.value, disciplineScalatest.value, scalacheckShapeless.value))

lazy val law = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, profunctor)
  .settings(stdProjectSettings("law"))
  .settings(libraryDependencies ++= Seq(cats.value, spire.value, catsLaws.value, discipline.value, disciplineScalatest.value))

lazy val test = crossProject(JVMPlatform, JSPlatform)
  .dependsOn(core, profunctor, law)
  .settings(stdProjectSettings("test"))
  .settings(libraryDependencies ++= Seq(cats.value, catsLaws.value, spire.value, discipline.value, disciplineScalatest.value, scalacheckShapeless.value))

lazy val docs = project
  .in(file("docs"))
  .dependsOn(core.jvm, profunctor.jvm, law.jvm)
  .settings(moduleName := "proptics-docs")
  .settings(noPublishSettings)
  .settings(stdSettings)
  .settings(mdocSettings(latestVersion, core.jvm, profunctor.jvm, law.jvm))
  .settings(buildInfoSettings(latestVersion, core.jvm))
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))
  .enablePlugins(BuildInfoPlugin, DocusaurusPlugin, MdocPlugin, ScalaUnidocPlugin)
