package com.al333z.antitest.kernel

import com.al333z.antitest.LoggerT

import scala.language.higherKinds

trait TestScenario[F[_], FeatureDeps] {
  type ScenarioDeps
  type Sample

  val description: String

  def before(featureDeps: FeatureDeps): ScenarioDeps

  def after(scenarioDeps: ScenarioDeps): Unit = ()

  def behaviour(featureDeps: FeatureDeps, scenarioDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit]
}

case class Scenario[F[_], FeatureDeps, DS](name: String,
                                           beforeStep: FeatureDeps => DS,
                                           step: (FeatureDeps, DS) => LoggerT[F, Vector[String], Unit],
                                           afterStep: DS => Unit = (_: DS) => ()) extends TestScenario[F, FeatureDeps] {
  override type ScenarioDeps = DS
  override type Sample = Unit

  override val description: String = name

  override def before(featureDeps: FeatureDeps): ScenarioDeps = beforeStep(featureDeps)

  override def behaviour(featureDeps: FeatureDeps, scenarioDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit] =
    step(featureDeps, scenarioDeps)

  override def after(scenarioDeps: ScenarioDeps): Unit = afterStep(scenarioDeps)
}

case class SampleScenario[F[_], FeatureDeps, DS, Ex](name: String,
                                                     beforeStep: FeatureDeps => DS,
                                                     step: (FeatureDeps, DS, Ex) => LoggerT[F, Vector[String], Unit],
                                                     afterStep: DS => Unit = (_: DS) => (),
                                                     sample: Ex) extends TestScenario[F, FeatureDeps] {
  override type ScenarioDeps = DS
  override type Sample = Ex

  override val description: String = name

  override def before(featureDeps: FeatureDeps): ScenarioDeps = beforeStep(featureDeps)

  override def behaviour(featureDeps: FeatureDeps, scenarioDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit] =
    step(featureDeps, scenarioDeps, sample)

  override def after(scenarioDeps: ScenarioDeps): Unit = afterStep(scenarioDeps)
}
