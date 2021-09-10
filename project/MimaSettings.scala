import BuildHelper.isScala3
import com.typesafe.tools.mima.plugin.MimaKeys._
import sbt.Keys.{moduleName, organization, scalaVersion}
import sbt._

object MimaSettings {
  lazy val previousArtifactsToCompare = "0.3.1"
  def mimaSettings(failOnProblem: Boolean) = Seq(
    mimaFailOnProblem := failOnProblem,
    mimaPreviousArtifacts := {
      if (isScala3(scalaVersion.value)) Set()
      else Set(organization.value %% moduleName.value % previousArtifactsToCompare)
    }
  )
}
