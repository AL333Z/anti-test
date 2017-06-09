package com.al333z.antitest

import com.al333z.antitest.kernel.{Feature, Scenario}

import scala.language.higherKinds

object TestBuilders {

  def testSuite[F[_], FeatureDeps](name: String)
                                  (beforeAll: => FeatureDeps,
                                   scenarios: Seq[Scenario[F, FeatureDeps]],
                                   afterAll: FeatureDeps => Unit = (_: FeatureDeps) => ()): Feature[F, FeatureDeps] =
    Feature[F, FeatureDeps](name, beforeAll, scenarios, afterAll)

  def test[F[_], FeatureDeps, ScenarioDep](name: String)
                                          (before: FeatureDeps => ScenarioDep,
                                           scenario: (FeatureDeps, ScenarioDep) => LoggerT[F, Vector[String], Unit],
                                           after: ScenarioDep => Unit = (_: ScenarioDep) => ()): Scenario[F, FeatureDeps] =
    Scenario(name, before, scenario, after)

  def test[F[_], FeatureDeps](name: String)
                             (scenario: FeatureDeps => LoggerT[F, Vector[String], Unit]): Scenario[F, FeatureDeps] =
    Scenario(name, (_: FeatureDeps) => (), (x: FeatureDeps, _: Unit) => scenario(x))
}
