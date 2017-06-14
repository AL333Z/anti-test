package com.al333z.antitest

import com.al333z.antitest.kernel.{Feature, SampleScenario, Scenario, TestScenario}

import scala.language.higherKinds

object TestBuilders {

  def testSuite[F[_], FeatureDeps](name: String)
                                  (beforeAll: => FeatureDeps,
                                   scenarios: Seq[TestScenario[F, FeatureDeps]],
                                   afterAll: FeatureDeps => Unit = (_: FeatureDeps) => ()): Feature[F, FeatureDeps] =
    Feature[F, FeatureDeps](name, beforeAll, scenarios, afterAll)

  def test[F[_], FeatureDeps, ScenarioDep](name: String)
                                          (before: FeatureDeps => ScenarioDep,
                                           scenario: (FeatureDeps, ScenarioDep) => LoggerT[F, Vector[String], Unit],
                                           after: ScenarioDep => Unit = (_: ScenarioDep) => ()): TestScenario[F, FeatureDeps] =
    Scenario(name, before, scenario, after)

  def test[F[_], FeatureDeps](name: String)
                             (scenario: FeatureDeps => LoggerT[F, Vector[String], Unit]): TestScenario[F, FeatureDeps] =
    Scenario(name, (_: FeatureDeps) => (), (x: FeatureDeps, _: Unit) => scenario(x))

  def tests[F[_], FeatureDeps, ScenarioDep, Ex](name: String)
                                               (before: FeatureDeps => ScenarioDep,
                                                scenario: (FeatureDeps, ScenarioDep, Ex) => LoggerT[F, Vector[String], Unit],
                                                after: ScenarioDep => Unit = (_: ScenarioDep) => (),
                                                examples: Seq[Ex]): Seq[TestScenario[F, FeatureDeps]] =
    examples.map(ex => SampleScenario[F, FeatureDeps, ScenarioDep, Ex](name, before, scenario, after, ex))

  def tests[F[_], FeatureDeps, Ex](name: String)
                                  (scenario: (FeatureDeps, Ex) => LoggerT[F, Vector[String], Unit],
                                   examples: Seq[Ex]): Seq[TestScenario[F, FeatureDeps]] =
    examples.map(ex => SampleScenario[F, FeatureDeps, Unit, Ex](
      name = name,
      beforeStep = (_: FeatureDeps) => (),
      step = (x: FeatureDeps, _: Unit, xx: Ex) => scenario(x, xx),
      sample = ex)
    )

}
