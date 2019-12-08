ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.github.sagifogel"

val buildSettings = Seq(
  name := "Optics",
  version := "0.1",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
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
  ),
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
  addCompilerPlugin(kindProjector))

lazy val kindProjector  = "org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full
lazy val opticsSettings = buildSettings
lazy val optics = project.in(file("."))
  .settings(moduleName := "optics")
  .settings(opticsSettings)



