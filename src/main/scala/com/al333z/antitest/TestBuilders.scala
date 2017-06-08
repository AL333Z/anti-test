package com.al333z.antitest

import com.al333z.antitest.kernel.{Feature, Scenario}

import scala.language.higherKinds

object TestBuilders {

  def testSuite[F[_], Deps](name: String)
                           (beforeAll: => Deps,
                            tearDown: Deps => Unit = (_: Deps) => (),
                            scenarios: Seq[Scenario[F, Deps]]): Feature[F, Deps] =
    Feature[F, Deps](name, beforeAll, tearDown, scenarios)

  def test[F[_], FeatureDeps, ScenarioDep](name: String)
                                          (before: FeatureDeps => ScenarioDep,
                                           step: (FeatureDeps, ScenarioDep) => LoggerT[F, Vector[String], Unit]): Scenario[F, FeatureDeps] =
    Scenario(name, before, step)

  def test[F[_], FeatureDeps](name: String)
                             (step: (FeatureDeps, Unit) => LoggerT[F, Vector[String], Unit]): Scenario[F, FeatureDeps] =
    Scenario(name, (_: FeatureDeps) => (), step)
}
