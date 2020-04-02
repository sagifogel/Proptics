import sbtcrossproject.CrossPlugin.autoImport.CrossType

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.github.sagifogel"

lazy val cats = Def.setting("org.typelevel" %% "cats-core" % "2.0.0")
lazy val spire = Def.setting("org.typelevel" %% "spire" % "0.17.0-M1")
lazy val catsMtl = Def.setting("org.typelevel" %% "cats-mtl-core" % "0.7.0")
lazy val kindProjector = "org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full

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

lazy val proptics = project.in(file("."))
  .settings(moduleName := "proptics")
  .settings(propticsSettings)



