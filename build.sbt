import BuildHelper._
import Dependencies._
import MimaSettings.mimaSettings
import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / latestVersion := {
  val snapshot = (ThisBuild / isSnapshot).value
  val stable = (ThisBuild / isVersionStable).value

  if (!snapshot && stable) {
    (ThisBuild / version).value
  } else {
    (ThisBuild / previousStableVersion).value.getOrElse("0.0.0")
  }
}

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
    )
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
  .aggregate(core.jvm, profunctor.jvm, macros.jvm, law.jvm, test.jvm, example)
  .dependsOn(core.jvm, profunctor.jvm, macros.jvm, law.jvm, test.jvm)

lazy val propticsJS = project
  .in(file(".propticsJS"))
  .settings(noPublishSettings)
  .aggregate(core.js, profunctor.js, macros.js, law.js, test.js)
  .dependsOn(core.js, profunctor.js, macros.js, law.js, test.js)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(profunctor)
  .settings(stdProjectSettings("core"))
  .settings(crossProjectSettings)
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))

lazy val coreJS = core.js
lazy val coreJVM = core.jvm.settings(mimaSettings(false))

lazy val profunctor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(stdProjectSettings("profunctor"))
  .settings(crossProjectSettings)
  .settings(libraryDependencies ++= Seq(cats.value))

lazy val profunctorJS = profunctor.js
lazy val profunctorJVM = profunctor.jvm.settings(mimaSettings(false))

lazy val macros = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("macros"))
  .dependsOn(core)
  .settings(crossProjectSettings)
  .settings(stdProjectSettings("macros"))
  .settings(macroDefinitionSettings)

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

lazy val example = project
  .dependsOn(core.jvm, profunctor.jvm, macros.jvm, test.jvm % "test->test")
  .settings(noPublishSettings)
  .settings(stdProjectSettings("example"))
  .settings(libraryDependencies ++= Seq(scalaCompiler.value, cats.value, catsLaws.value, spire.value, discipline.value, disciplineScalatest.value, scalacheckShapeless.value))

lazy val law = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, profunctor)
  .settings(stdProjectSettings("law"))
  .settings(crossProjectSettings)
  .settings(libraryDependencies ++= Seq(cats.value, spire.value, catsLaws.value, discipline.value, disciplineScalatest.value))

lazy val test = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, profunctor, macros, law)
  .settings(stdProjectSettings("test"))
  .settings(crossProjectSettings)
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
