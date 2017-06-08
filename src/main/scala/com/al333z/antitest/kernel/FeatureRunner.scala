package com.al333z.antitest.kernel

import cats.{Comonad, MonadError}
import org.scalatest.FeatureSpec

import scala.language.higherKinds
import scala.util.Try

trait FeatureRunner[F[_]] {
  featureSpec: FeatureSpec =>

  def runFeature[FeatureDeps, E](feature: Feature[F, FeatureDeps])(implicit me: MonadError[F, E], cm: Comonad[F]): Unit = {
    featureSpec.feature(feature.description) {
      val configuration: FeatureDeps = feature.beforeAll
      feature.scenarios.foreach {
        scenario =>
          featureSpec.scenario(scenario.description) {

            val resource: scenario.ScenarioDeps = scenario.before(configuration)
            val behaviour = Try(cm.extract(scenario.behaviour(configuration, resource).run)).toEither

            scenario.after(resource)
            verify(behaviour)
          }
      }
      feature.afterAll(configuration)
    }
  }

  private def verify(testResult: Either[Throwable, (Vector[String], Unit)]) = {
    testResult match {
      case Right((steps, _)) => {
        println(steps.mkString("\n") + "\nPASSED\n")
        assert(true)
      }
      case Left(t) => {
        fail(t.getMessage + "\nFAILED\n", t)
      }
    }
  }
}
