package com.al333z.antitest.kernel

import scala.language.higherKinds

trait Feature[F[_], FeatureDeps] {
  val description: String

  def scenarios: Seq[Scenario[F, FeatureDeps]]

  def beforeAll: FeatureDeps

  def afterAll(dep: FeatureDeps): Unit = ()
}

object Feature {
  def apply[F[_], D](name: String, deps: => D, tearDown: D => Unit, scenario: Seq[Scenario[F, D]]): Feature[F, D] =
    new Feature[F, D] {
      override val description: String = name

      override def scenarios: Seq[Scenario[F, D]] = scenario

      override def beforeAll: D = deps

      override def afterAll(dep: D): Unit = tearDown(dep)
    }
}
