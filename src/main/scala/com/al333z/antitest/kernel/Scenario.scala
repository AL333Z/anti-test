package com.al333z.antitest.kernel

import com.al333z.antitest.LoggerT

import scala.language.higherKinds

trait Scenario[F[_], FeatureDeps] {
  type ScenarioDeps

  val description: String

  def before(featureDeps: FeatureDeps): ScenarioDeps

  def after(scenarioDeps: ScenarioDeps): Unit = ()

  def behaviour(featureDeps: FeatureDeps, scenarioDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit]
}

object Scenario {
  def apply[F[_], FeatureDeps, DS](name: String,
                                   beforeStep: FeatureDeps => DS,
                                   step: (FeatureDeps, DS) => LoggerT[F, Vector[String], Unit],
                                   afterStep: DS => Unit = (_: DS) => ()): Scenario[F, FeatureDeps] =
    new Scenario[F, FeatureDeps] {
      override type ScenarioDeps = DS
      override val description: String = name

      override def before(featureDeps: FeatureDeps): ScenarioDeps = beforeStep(featureDeps)

      override def behaviour(featureDeps: FeatureDeps, scenarioDeps: ScenarioDeps): LoggerT[F, Vector[String], Unit] =
        step(featureDeps, scenarioDeps)

      override def after(scenarioDeps: ScenarioDeps): Unit = afterStep(scenarioDeps)
    }
}
