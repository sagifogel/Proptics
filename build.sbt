import sbt.Keys.{baseDirectory, _}
import sbtunidoc.BaseUnidocPlugin.autoImport._

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "com.github.sagifogel",
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

val Scala212 = "2.12.13"
val Scala213 = "2.13.5"
val catsVersion = "2.6.0"
lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % catsVersion)
lazy val catsLaws = Def.setting("org.typelevel" %%% "cats-laws" % catsVersion)
lazy val spire = Def.setting("org.typelevel" %%% "spire" % "0.17.0")
lazy val discipline = Def.setting("org.typelevel" %%% "discipline-core" % "1.1.4")
lazy val disciplineScalatest = Def.setting("org.typelevel" %%% "discipline-scalatest" % "2.1.4")
lazy val scalacheckShapeless = Def.setting("com.github.alexarchambault" %%% "scalacheck-shapeless_1.14" % "1.2.5")
lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full
lazy val gitRev = sys.process.Process("git rev-parse HEAD").lineStream_!.head

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

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  skip / publish := true
)

def revisionToUse = Def.task {
  val tag = (ThisBuild / version).value
  if (isSnapshot.value) gitRev else tag
}

lazy val scalajsSettings = Seq(
  scalacOptions += {
    lazy val tag = (ThisBuild / version).value
    val s = if (isSnapshot.value) gitRev else tag
    val a = (LocalRootProject / baseDirectory).value.toURI.toString
    val g = "https://raw.githubusercontent.com/sagifogel/Proptics"
    s"-P:scalajs:mapSourceURI:$a->$g/$s/"
  },
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "8", "-minSuccessfulTests", "50")
)

lazy val propticsSettings = Seq(
  scalaVersion := Scala213,
  crossScalaVersions := Seq(Scala212, Scala213),
  scalacOptions ++= commonScalacOptions(scalaVersion.value),
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  addCompilerPlugin(kindProjector),
  addCompilerPlugin(scalafixSemanticdb),
  Compile / console / scalacOptions ~= {
    _.filterNot(Set("-Xfatal-warnings", "-Xlint", "-Ywarn-unused:imports"))
  },
  Test / console / scalacOptions ~= {
    _.filterNot(Set("-Xfatal-warnings", "-Xlint", "-Ywarn-unused:imports"))
  },
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  scmInfo := Some(ScmInfo(url("https://github.com/sagifogel/Proptics"), "scm:git:git@github.com:sagifogel/Proptics.git"))
)

lazy val propticsJVMSettings = propticsSettings ++ Seq(skip / publish := true) ++ Seq(Test / fork := true)
lazy val propticsJSSettings = propticsSettings ++ scalajsSettings

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _ => false
  }

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String): List[sbt.File] =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))

  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) => extraDirs("-2.x") ++ (if (y >= 13) extraDirs("-2.13+") else Nil)
    case Some((0 | 3, _)) => extraDirs("-2.13+") ++ extraDirs("-3.x")
    case _ => Nil
  }
}
def commonScalacOptions(scalaVersion: String): Seq[String] =
  Seq(
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-deprecation",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Yrangepos"
  ) ++
    (if (priorTo2_13(scalaVersion))
       Seq("-Yno-adapted-args", "-Ypartial-unification", "-Xfuture", "-Ywarn-unused-import")
     else
       Seq("-Ymacro-annotations", "-Ywarn-unused:imports"))

lazy val proptics = project
  .in(file("."))
  .settings(moduleName := "proptics")
  .settings(noPublishSettings)
  .settings(propticsSettings)
  .aggregate(propticsJVM, propticsJS)
  .dependsOn(propticsJVM, propticsJS)
  .enablePlugins(ScalaJSPlugin)

lazy val propticsJVM = project
  .in(file(".propticsJVM"))
  .settings(propticsJVMSettings)
  .settings(noPublishSettings)
  .aggregate(core.jvm, profunctor.jvm, newtype.jvm, law.jvm, test.jvm, example)
  .dependsOn(core.jvm, profunctor.jvm, newtype.jvm, law.jvm, test.jvm)

lazy val propticsJS = project
  .in(file(".propticsJS"))
  .settings(propticsJSSettings)
  .settings(noPublishSettings)
  .aggregate(core.js, profunctor.js, newtype.js, law.js, test.js)
  .dependsOn(core.js, profunctor.js, newtype.js, law.js, test.js)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .configureCross(_.jvmSettings(propticsJVMSettings), _.jsSettings(propticsJSSettings))
  .dependsOn(profunctor)
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))
  .settings(
    moduleName := "proptics-core",
    name := "Proptics core",
    scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))) // Workaround for sbt bug
  )

lazy val profunctor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "proptics-profunctor", name := "Proptics profunctor")
  .settings(propticsSettings)
  .configureCross(_.jvmSettings(propticsJVMSettings), _.jsSettings(propticsJSSettings))
  .settings(libraryDependencies ++= Seq(cats.value))

lazy val newtype = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "proptics-newtype", name := "Proptics newtype")
  .settings(propticsSettings)
  .configureCross(_.jvmSettings(propticsJVMSettings), _.jsSettings(propticsJSSettings))
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))

lazy val example = project
  .dependsOn(core.jvm, profunctor.jvm, newtype.jvm, test.jvm % "test->test")
  .settings(moduleName := "proptics-example")
  .settings(propticsJVMSettings)
  .settings(noPublishSettings)
  .settings(libraryDependencies ++= Seq(cats.value, catsLaws.value, spire.value, discipline.value, disciplineScalatest.value, scalacheckShapeless.value))

lazy val law = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core, profunctor, newtype)
  .settings(moduleName := "proptics-law", name := "Proptics law")
  .configureCross(
    _.jvmSettings(propticsJVMSettings),
    _.jsSettings(propticsJSSettings)
  )
  .settings(libraryDependencies ++= Seq(cats.value, spire.value, catsLaws.value, discipline.value, disciplineScalatest.value))

lazy val test = crossProject(JVMPlatform, JSPlatform)
  .dependsOn(core, profunctor, newtype, law)
  .settings(
    moduleName := "proptics-test",
    name := "Proptics test",
    scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))) // Workaround for sbt bug
  )
  .configureCross(
    _.jvmSettings(propticsJVMSettings),
    _.jsSettings(propticsJSSettings)
  )
  .settings(libraryDependencies ++= Seq(cats.value, catsLaws.value, spire.value, discipline.value, disciplineScalatest.value, scalacheckShapeless.value))

lazy val docs = project
  .in(file("docs"))
  .dependsOn(core.jvm, newtype.jvm, profunctor.jvm, law.jvm)
  .settings(moduleName := "proptics-docs")
  .settings(propticsSettings)
  .settings(noPublishSettings)
  .settings(mdocSettings)
  .settings(buildInfoSettings)
  .settings(scalacOptions ~= (_.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-dead-code"))))
  .settings(libraryDependencies ++= Seq(cats.value, spire.value))
  .enablePlugins(BuildInfoPlugin, DocusaurusPlugin, MdocPlugin, ScalaUnidocPlugin)

lazy val buildInfoSettings = Seq(
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
    BuildInfoKey.map(core.jvm / moduleName) { case (k, v) =>
      "core" ++ k.capitalize -> v
    },
    BuildInfoKey.map(core.jvm / crossScalaVersions) { case (k, v) =>
      "core" ++ k.capitalize -> v
    },
    LocalRootProject / organization,
    core.jvm / crossScalaVersions
  )
)

lazy val mdocSettings = Seq(
  mdoc := (Compile / run).evaluated,
  scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused"),
  crossScalaVersions := Seq(scalaVersion.value),
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(core.jvm, newtype.jvm, profunctor.jvm, law.jvm),
  ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
  cleanFiles += (ScalaUnidoc / unidoc / target).value,
  docusaurusCreateSite := docusaurusCreateSite
    .dependsOn(Compile / unidoc)
    .dependsOn(ThisBuild / updateSiteVariables)
    .value,
  docusaurusPublishGhpages :=
    docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .dependsOn(ThisBuild / updateSiteVariables)
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

def minorVersion(version: String): String = {
  val (major, minor) =
    CrossVersion.partialVersion(version).get
  s"$major.$minor"
}

val latestVersion = settingKey[String]("Latest stable released version")
ThisBuild / latestVersion := (ThisBuild / version).value

val updateSiteVariables = taskKey[Unit]("Update site variables")
ThisBuild / updateSiteVariables := {
  val file = (LocalRootProject / baseDirectory).value / "website" / "variables.js"

  val variables =
    Map[String, String](
      "organization" -> (LocalRootProject / organization).value,
      "coreModuleName" -> (core.jvm / moduleName).value,
      "latestVersion" -> (ThisBuild / latestVersion).value,
      "scalaPublishVersions" -> {
        val minorVersions = (core.jvm / crossScalaVersions).value.map(minorVersion)
        if (minorVersions.size <= 2) minorVersions.mkString(" and ")
        else minorVersions.init.mkString(", ") ++ " and " ++ minorVersions.last
      }
    )
  val fileHeader =
    "// Generated by sbt. Do not edit directly."

  val fileContents =
    variables.toList
      .sortBy { case (key, _) => key }
      .map { case (key, value) => s"  $key: '$value'" }
      .mkString(s"$fileHeader\nmodule.exports = {\n", ",\n", "\n};\n")

  IO.write(file, fileContents)
}

ThisBuild / semanticdbEnabled := true
ThisBuild / sonatypeCredentialHost := Sonatype.sonatype01
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
