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
