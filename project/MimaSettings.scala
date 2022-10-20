import com.typesafe.tools.mima.plugin.MimaKeys._
import sbt.Keys.{moduleName, organization}
import sbt.{Def, _}

object MimaSettings {
  lazy val previousArtifactsToCompare = "0.5.1"
  def mimaSettings(failOnProblem: Boolean): Seq[Def.Setting[_]] = Seq(
    mimaFailOnProblem := failOnProblem,
    mimaPreviousArtifacts := Set(organization.value %% moduleName.value % previousArtifactsToCompare)
  )
}
