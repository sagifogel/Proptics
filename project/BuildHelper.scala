import mdoc.DocusaurusPlugin.autoImport._
import mdoc.MdocPlugin.autoImport._
import sbt.Keys.{scalacOptions, _}
import sbt.{Compile, CrossVersion, Def, Test, inProjects, settingKey, _}
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
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
  lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full
  lazy val disciplineScalatest = Def.setting("org.typelevel" %% "discipline-scalatest" % "2.1.5")
  lazy val scalacheckShapeless = Def.setting("com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5")
}

object BuildHelper {
  import Dependencies._

  val Scala213 = "2.13.5"
  val Scala212 = "2.12.13"
  lazy val latestVersion: SettingKey[String] = settingKey[String]("Latest stable released version")
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
    ThisBuild / sonatypeLogLevel := "debug",
    sonatypeCredentialHost := Sonatype.sonatype01,
    crossScalaVersions := Seq(Scala212, Scala213),
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbOptions += "-P:semanticdb:synthetics:on",
    Compile / doc / scalacOptions ~= removeScalaOptions,
    libraryDependencies ++= Seq(
      compilerPlugin(scalafixSemanticdb),
      compilerPlugin(Dependencies.kindProjector)
    ),
    scalacOptions := stdOptions ++ extraOptions(scalaVersion.value),
    ThisBuild / scalafixDependencies += organizeImports,
    ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
  )

  def stdProjectSettings(projectName: String): Seq[Def.Setting[_]] = Seq(
    name := s"Proptics $projectName",
    moduleName := s"proptics-$projectName"
  ) ++ stdSettings

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

  def mdocSettings(latestVersion: SettingKey[String], refernces: ProjectReference*) = Seq(
    mdoc := (Compile / run).evaluated,
    crossScalaVersions := Seq(scalaVersion.value),
    scalacOptions ~= removeScalaOptions,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(refernces: _*),
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
      s"https://github.com/sagifogel/Proptics/tree/v${(ThisBuild / latestVersion).value}€{FILE_PATH}.scala",
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-title",
      "Proptics",
      "-doc-version",
      s"v${(ThisBuild / latestVersion).value}"
    )
  )

  def buildInfoSettings(latestVersion: SettingKey[String], coreProject: ProjectReference) = Seq(
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
}
