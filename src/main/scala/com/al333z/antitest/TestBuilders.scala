package com.al333z.antitest

import com.al333z.antitest.kernel.{Feature, Scenario}

import scala.language.higherKinds

object TestBuilders {

  def testSuite[F[_], Deps](name: String)(deps: => Deps)
                           (scenario: Scenario[F, Deps]*): Feature[F, Deps] =
    new Feature[F, Deps] {
      override val description: String = name

      override def beforeAll: Deps = deps

      override def scenarios: Seq[Scenario[F, Deps]] = scenario
    }

  def test[F[_], FeatureDeps, ScenarioDep](name: String)
                                          (beforeStep: FeatureDeps => ScenarioDep)
                                          (step: (FeatureDeps, ScenarioDep) => LoggerT[F, Vector[String], Unit])
  : Scenario[F, FeatureDeps] = {
    new Scenario[F, FeatureDeps] {
      override type ScenarioDeps = ScenarioDep

      override val description: String = name

      override def before(deps: FeatureDeps): ScenarioDeps = beforeStep(deps)

      override def behaviour(deps: FeatureDeps, stepDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit] =
        step(deps, stepDeps)
    }
  }

}
