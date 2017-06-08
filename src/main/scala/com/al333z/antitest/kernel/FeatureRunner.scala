package com.al333z.antitest.kernel

import cats.{Comonad, MonadError}
import org.scalatest.FeatureSpecLike

import scala.language.higherKinds
import scala.util.Try

trait FeatureRunner[F[_]] extends FeatureSpecLike {

  def runFeature[FeatureDeps](f: Feature[F, FeatureDeps])(implicit me: MonadError[F, Vector[String]], cm: Comonad[F]): Unit = {
    feature(f.description) {
      val configuration: FeatureDeps = f.beforeAll
      f.scenarios.foreach {
        s =>
          scenario(s.description) {

            val resource: s.ScenarioDeps = s.before(configuration)
            val behaviour = Try(cm.extract(s.behaviour(configuration, resource).run)).toEither

            s.after(resource)
            verify(behaviour)
          }
      }
      f.afterAll(configuration)
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
