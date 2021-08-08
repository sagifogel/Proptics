import mdoc.DocusaurusPlugin.autoImport._
import mdoc.MdocPlugin.autoImport._
import sbt.Keys._
import sbt.{Compile, CrossVersion, Def, Test, inProjects, settingKey, _}
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport._
import sbtunidoc.BaseUnidocPlugin.autoImport._
import sbtunidoc.ScalaUnidocPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin.autoImport._
import xerial.sbt.Sonatype
import xerial.sbt.Sonatype.autoImport._

object Dependencies {
  val catsVersion = "2.6.1"
  lazy val spire = Def.setting("org.typelevel" %% "spire" % "0.17.0")
  lazy val cats = Def.setting("org.typelevel" %% "cats-core" % catsVersion)
  lazy val catsLaws = Def.setting("org.typelevel" %% "cats-laws" % catsVersion)
  lazy val discipline = Def.setting("org.typelevel" %% "discipline-core" % "1.1.5")
  lazy val organizeImports = "com.github.liancheng" %% "organize-imports" % "0.5.0"
  lazy val scalaReflect = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  lazy val scalaLibrary = Def.setting("org.scala-lang" % "scala-library" % scalaVersion.value)
  lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full
  lazy val scalaCompiler = Def.setting("org.scala-lang" % "scala-compiler" % scalaVersion.value)
  lazy val disciplineScalatest = Def.setting("org.typelevel" %% "discipline-scalatest" % "2.1.5")
  lazy val scalacheckShapeless = Def.setting("com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0")
}

object BuildHelper {
  import Dependencies._

  val Scala213 = "2.13.6"
  val Scala212 = "2.12.14"
  val latestVersion: SettingKey[String] = settingKey[String]("Latest stable released version")
  private val sonatypeRepo = s"https://${Sonatype.sonatype01}/service/local"
  private val stdOptions =
    Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-Yrangepos",
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-Xfatal-warnings",
      "-Ywarn-dead-code",
      "Ywarn-numeric-widen",
      "-language:postfixOps",
      "-Ywarn-value-discard",
      "-language:higherKinds",
      "-language:existentials",
      "-language:implicitConversions",
      "Ywarn-unused:params,-implicits"
    )

  def setLatestVersion: Def.Setting[String] =
    ThisBuild / latestVersion := {
      val snapshot = (ThisBuild / isSnapshot).value
      val stable = (ThisBuild / isVersionStable).value

      if (!snapshot && stable) {
        (ThisBuild / version).value
      } else {
        (ThisBuild / previousStableVersion).value.getOrElse("0.0.0")
      }
    }

  private def extraOptions(scalaVersion: String): Seq[String] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) =>
        Seq("-Ywarn-unused:imports")
      case Some((2, 12)) =>
        Seq(
          "-Xfuture",
          "-Yno-adapted-args",
          "-Ywarn-unused-import",
          "-Ypartial-unification"
        )
      case _ =>
        Seq.empty[String]
    }

  lazy val noPublishSettings =
    Seq(
      publish := {},
      publishLocal := {},
      publishArtifact := false,
      skip / publish := true
    )

  lazy val stdSettings = Seq(
    semanticdbEnabled := true,
    Test / parallelExecution := true,
    sonatypeRepository := sonatypeRepo,
    ThisBuild / scalaVersion := Scala213,
    sonatypeCredentialHost := Sonatype.sonatype01,
    crossScalaVersions := Seq(Scala212, Scala213),
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbOptions += "-P:semanticdb:synthetics:on",
    ThisBuild / scalafixDependencies += organizeImports,
    Compile / doc / scalacOptions ~= removeScalaOptions,
    libraryDependencies ++= Seq(
      compilerPlugin(scalafixSemanticdb),
      compilerPlugin(Dependencies.kindProjector)
    ),
    scalacOptions := stdOptions ++ extraOptions(scalaVersion.value),
    ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
  )

  def stdProjectSettings(projectName: String): Seq[Def.Setting[_]] = Seq(
    name := s"Proptics $projectName",
    moduleName := s"proptics-$projectName"
  ) ++ stdSettings

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= Seq(scalaCompiler.value, scalaLibrary.value, scalaReflect.value)
  )

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*): List[File] =
    for {
      platform <- List("shared", platform)
      version <- "scala" :: versions.toList.map("scala-" + _)
      result = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File): List[File] = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case _ =>
        List()
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "main",
        baseDirectory.value
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "test",
        baseDirectory.value
      )
    }
  )

  def removeScalaOptions: Seq[String] => Seq[String] =
    _.filterNot(Set("-Xfatal-warnings", "Ywarn-numeric-widen", "-Ywarn-dead-code"))
      .filterNot(_.startsWith("-Wconf"))
      .filterNot(_.contains("Ywarn-unused"))

  def mdocSettings(references: ProjectReference*) = Seq(
    mdoc := (Compile / run).evaluated,
    scalacOptions ~= removeScalaOptions,
    crossScalaVersions := Seq(scalaVersion.value),
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(references: _*),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite
      .dependsOn(Compile / unidoc)
      .value,
    docusaurusPublishGhpages :=
      docusaurusPublishGhpages
        .dependsOn(Compile / unidoc)
        .value,
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-doc-source-url",
      s"https://github.com/sagifogel/Proptics/tree/${(ThisBuild / latestVersion).value}€{FILE_PATH}.scala",
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-title",
      "Proptics",
      "-doc-version",
      s"${(ThisBuild / latestVersion).value}"
    )
  )

  def buildInfoSettings(coreProject: ProjectReference) = Seq(
    buildInfoPackage := "proptics.build",
    buildInfoObject := "info",
    buildInfoKeys := Seq[BuildInfoKey](
      scalaVersion,
      scalacOptions,
      sourceDirectory,
      ThisBuild / latestVersion,
      BuildInfoKey.map(ThisBuild / version) { case (_, v) =>
        "latestSnapshotVersion" -> v
      },
      BuildInfoKey.map(coreProject / moduleName) { case (k, v) =>
        "core" ++ k.capitalize -> v
      },
      BuildInfoKey.map(coreProject / crossScalaVersions) { case (k, v) =>
        "core" ++ k.capitalize -> v
      },
      LocalRootProject / organization,
      coreProject / crossScalaVersions
    )
  )

  def welcomeMessage: Def.Setting[String] = onLoadMessage := {
    import scala.{Console => Consl}

    def header(text: String): String = s"${Consl.BLUE}$text${Consl.RESET}"

    def item(text: String): String = s"${Consl.MAGENTA}> ${Consl.BLUE}$text${Consl.RESET}"

    s"""|${header("""    ____                   __  _           ___                ___""")}
        |${header("""   / __ \_________  ____  / /_(_)_________/ _/               /  /""")}
        |${header("""  / /_/ / ___/ __ \/ __ \/ __/ / ___/ ___/ /                 / /""")}
        |${header(""" / ____/ /  / /_/ / /_/ / /_/ / /__(__  ) /     _           / /""")}
        |${header("""/_/   /_/   \____/  ___/\__/_/\___/____/ /_____( )  _______/ /""")}
        |${header(s"                /_/                   /__/_____//  /_____/__/   ${(ThisBuild / latestVersion).value}")}
        |
        |Useful sbt tasks:
        |${item("build")} - Prepares sources, compiles and runs tests.
        |${item("prepare")} - Prepares sources by applying both scalafix and scalafmt
        |${item("fix")} - Fixes sources files using scalafix
        |${item("fmt")} - Formats source files using scalafmt
        |${item("testJVM")} - Runs all JVM tests
        |${item("testJS")} - Runs all ScalaJS tests
     """.stripMargin
  }
}
