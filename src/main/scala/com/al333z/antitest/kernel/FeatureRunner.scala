package com.al333z.antitest.kernel

import cats.{Comonad, Eval, MonadError}
import org.scalatest.FeatureSpecLike

import scala.collection.immutable
import scala.language.higherKinds
import scala.util.Try

trait FeatureRunner[F[_]] extends FeatureSpecLike {

  def runFeature[FeatureDeps](f: Feature[F, FeatureDeps])
                             (implicit me: MonadError[F, Vector[String]], cm: Comonad[F]): Unit = feature(f.description) {

    val featureDeps = Eval.later(f.beforeAll())
    val scenarios = f.scenarios
    val indexes = scenarios.indices

    scenarios.zip(indexes).foreach {
      case (s, i) =>
        val description = i + ". " + s.description + {
          s match {
            case SampleScenario(_, _, _, _, sample) ⇒ " " + sample
            case _ ⇒ ""
          }
        }
        scenario(description) {
          val fvalue = featureDeps.value
          val scenarioDeps: s.ScenarioDeps = s.before(fvalue)
          val result = Try(cm.extract(s.behaviour(fvalue, scenarioDeps).run)).toEither

          try {
            s.after(scenarioDeps)
            if(i == (scenarios.length -1))
              f.afterAll(fvalue) // FIXME we can do better than this
          }
          finally {
            verify(result)
          }
        }
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
