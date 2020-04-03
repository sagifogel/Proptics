import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.github.sagifogel"

lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.0.0")
lazy val spire = Def.setting("org.typelevel" %%% "spire" % "0.17.0-M1")
lazy val catsMtl = Def.setting("org.typelevel" %%% "cats-mtl-core" % "0.7.0")
lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full
lazy val gitRev = sys.process.Process("git rev-parse HEAD").lineStream_!.head
lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  skip in publish := true
)

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _ => false
  }

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
  def extraDirs(suffix: String) =
    List(CrossType.Pure, CrossType.Full)
      .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))

  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) if y <= 12 =>
      extraDirs("-2.12-")
    case Some((2, y)) if y >= 13 =>
      extraDirs("-2.13+")
    case _ => Nil
  }
}

def commonScalacOptions(scalaVersion: String) =
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
    "-Ywarn-unused:imports"
  ) ++ (if (priorTo2_13(scalaVersion))
    Seq("-Yno-adapted-args", "-Ypartial-unification", "-Xfuture")
  else
    Seq("-Ymacro-annotations"))

lazy val propticsSettings = Seq(
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq("2.12.10", "2.13.1"),
  scalacOptions ++= commonScalacOptions(scalaVersion.value),
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  resolvers ++= Seq(Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots")),
  parallelExecution in Test := false,
  addCompilerPlugin(kindProjector),
  scmInfo := Some(ScmInfo(url("https://github.com/sagifogel/Proptics"), "scm:git:git@github.com:sagifogel/Proptics.git")))

lazy val scalajsSettings = Seq(
  scalacOptions += {
    lazy val tag = (version in ThisBuild).value
    val s = if (isSnapshot.value) gitRev else tag
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/sagifogel/Proptics"
    s"-P:scalajs:mapSourceURI:$a->$g/$s/"
  },
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "8", "-minSuccessfulTests", "50")
)

lazy val propticsJVMSettings = propticsSettings ++ Seq(skip.in(publish) := false)
lazy val propticsJSSettings  = propticsSettings ++ scalajsSettings

lazy val proptics = project.in(file("."))
  .settings(moduleName := "proptics")
  .settings(propticsSettings)
  .settings(noPublishSettings)
  .aggregate(propticsJVM, propticsJS)
  .dependsOn(propticsJVM, propticsJS)

lazy val propticsJVM = project.in(file(".propticsJVM"))
  .settings(propticsJVMSettings)
  .settings(noPublishSettings)
  .aggregate(core.jvm)
  .dependsOn(core.jvm)

lazy val propticsJS = project.in(file(".propticsJS"))
  .settings(propticsJSSettings)
  .settings(noPublishSettings)
  .aggregate(core.js)
  .dependsOn(core.js)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .settings(moduleName := "proptics-core", name := "Proptics core")
  .configureCross(
    _.jvmSettings(propticsJVMSettings),
    _.jsSettings(propticsJSSettings),
  )
  .settings(libraryDependencies ++= Seq(cats.value, catsMtl.value, spire.value))
  .aggregate(profunctor)
  .dependsOn(profunctor)

lazy val profunctor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "proptics-profunctor", name:= "Proptics profunctor")
  .settings(propticsSettings)
  .configureCross(_.jvmSettings(propticsJVMSettings), _.jsSettings(propticsJSSettings))
  .settings(libraryDependencies ++= Seq(cats.value, catsMtl.value))


