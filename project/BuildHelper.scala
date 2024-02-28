import mdoc.DocusaurusPlugin.autoImport._
import mdoc.MdocPlugin.autoImport._
import sbt.Keys.{scalaVersion, _}
import sbt.{Compile, CrossVersion, Def, Test, inProjects, settingKey, _}
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtunidoc.BaseUnidocPlugin.autoImport._
import sbtunidoc.ScalaUnidocPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin.autoImport._
import xerial.sbt.Sonatype
import xerial.sbt.Sonatype.autoImport._

object Dependencies {
  val catsVersion = "2.10.0"
  lazy val spireDotty = Def.setting("org.typelevel" %% "spire" % "0.18.0")
  lazy val spireLegacy = Def.setting("org.typelevel" %% "spire" % "0.17.0")
  lazy val cats = Def.setting("org.typelevel" %% "cats-core" % catsVersion)
  lazy val catsLaws = Def.setting("org.typelevel" %% "cats-laws" % catsVersion)
  lazy val discipline = Def.setting("org.typelevel" %% "discipline-core" % "1.5.1")
  lazy val organizeImports = "com.github.liancheng" %% "organize-imports" % "0.6.0"
  lazy val scalaReflect = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  lazy val scalaLibrary = Def.setting("org.scala-lang" % "scala-library" % scalaVersion.value)
  lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
  lazy val scalaCompiler = Def.setting("org.scala-lang" % "scala-compiler" % scalaVersion.value)
  lazy val disciplineScalatest = Def.setting("org.typelevel" %% "discipline-scalatest" % "2.1.5")
}

object BuildHelper {
  import Dependencies._

  val Scala213 = "2.13.13"
  val Scala212 = "2.12.16"
  val ScalaDotty = "3.3.1"
  val scalaDottyVersions: Seq[String] = Seq(ScalaDotty)
  val latestVersion: SettingKey[String] = settingKey[String]("Latest stable released version")
  private val sonatypeRepo = s"https://${Sonatype.sonatype01}/service/local"
  private def stdOptions(scalaVersion: String): Seq[String] =
    if (isScala3(scalaVersion))
      Seq(
        "-Ykind-projector",
        "-Xignore-scala2-macros",
        "-language:implicitConversions,higherKinds,postfixOps"
      )
    else
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

  def isScala3(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion).exists(_._1 == 3)

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

  lazy val noPublishSettings: Seq[Def.Setting[_]] =
    Seq(
      publish := {},
      publishLocal := {},
      publishArtifact := false,
      skip / publish := true
    )

  lazy val stdSettings: Seq[Def.Setting[_]] =
    Seq(
      Test / parallelExecution := true,
      sonatypeRepository := sonatypeRepo,
      ThisBuild / scalaVersion := Scala213,
      sonatypeCredentialHost := Sonatype.sonatype01,
      semanticdbVersion := scalafixSemanticdb.revision,
      semanticdbEnabled := true,
      ThisBuild / scalafixDependencies += organizeImports,
      Compile / doc / scalacOptions ~= removeScalaOptions,
      libraryDependencies ++= {
        if (isScala3(scalaVersion.value)) Seq()
        else Seq(compilerPlugin(kindProjector))
      },
      crossScalaVersions := Seq(Scala212, Scala213) ++ scalaDottyVersions,
      scalacOptions := stdOptions(scalaVersion.value) ++ extraOptions(scalaVersion.value),
      ThisBuild / scalafixScalaBinaryVersion := {
        if (isScala3(scalaVersion.value)) "2.12"
        else CrossVersion.binaryScalaVersion(scalaVersion.value)
      },
      ThisBuild / scalafixConfig.withRank(KeyRanks.Invisible) := {
        val fileName =
          if (isScala3(scalaVersion.value)) ".scalafix-scala3.conf"
          else ".scalafix-scala2.conf"
        Some(file(fileName))
      }
    )

  lazy val additionalDependencies: Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= {
      if (isScala3(scalaVersion.value)) Seq(spireDotty.value)
      else Seq(spireLegacy.value)
    }
  )

  def stdProjectSettings(projectName: String): Seq[Def.Setting[_]] = Seq(
    name := s"Proptics $projectName",
    moduleName := s"proptics-$projectName"
  ) ++ stdSettings

  def macroDefinitionSettings: Seq[Def.Setting[_]] =
    Seq(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= {
        if (isScala3(scalaVersion.value)) Seq()
        else Seq(scalaCompiler.value, scalaLibrary.value, scalaReflect.value)
      }
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
        List("2.12", "2.12+", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case Some((3, _)) =>
        List("dotty", "2.12+", "2.13+", "3.x")
      case _ =>
        List.empty[String]
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings: Seq[Def.Setting[Seq[File]]] = Seq(
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
      s"https://github.com/sagifogel/Proptics/tree/v0.5.2€{FILE_PATH}.scala",
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-title",
      "Proptics",
      "-doc-version",
      "v0.5.2"
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
