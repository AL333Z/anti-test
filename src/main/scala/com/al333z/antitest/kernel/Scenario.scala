package com.al333z.antitest.kernel

import com.al333z.antitest.LoggerT

import scala.language.higherKinds

trait Scenario[F[_], FeatureDeps] {
  type ScenarioDeps

  val description: String

  def before(beforeAll: FeatureDeps): ScenarioDeps

  def after(before: ScenarioDeps): Unit = ()

  def behaviour(beforeAll: FeatureDeps, before: ScenarioDeps): LoggerT[F, Vector[String], Unit]
}

object Scenario {
  def apply[F[_], D, DS](name: String,
                         beforeStep: D => DS,
                         b: (D, DS) => LoggerT[F, Vector[String], Unit],
                         afterStep: DS => Unit = (_: DS) => ()): Scenario[F, D] = new Scenario[F, D] {
    override type ScenarioDeps = DS
    override val description: String = name

    override def before(beforeAll: D): ScenarioDeps = beforeStep(beforeAll)

    override def behaviour(beforeAll: D, deps: ScenarioDeps): LoggerT[F, Vector[String], Unit] = b(beforeAll, deps)

    override def after(deps: ScenarioDeps): Unit = afterStep(deps)
  }
}
