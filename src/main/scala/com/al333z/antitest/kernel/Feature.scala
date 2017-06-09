package com.al333z.antitest.kernel

import scala.language.higherKinds

trait Feature[F[_], FeatureDeps] {
  val description: String

  def scenarios: Seq[Scenario[F, FeatureDeps]]

  def beforeAll(): FeatureDeps

  def afterAll(featureDeps: FeatureDeps): Unit = ()
}

object Feature {
  def apply[F[_], FeatureDeps](name: String,
                               beforeAllFeature: => FeatureDeps,
                               scenario: Seq[Scenario[F, FeatureDeps]],
                               afterAllFeature: FeatureDeps => Unit): Feature[F, FeatureDeps] =
    new Feature[F, FeatureDeps] {
      override val description: String = name

      override def scenarios: Seq[Scenario[F, FeatureDeps]] = scenario

      override def beforeAll(): FeatureDeps = beforeAllFeature

      override def afterAll(dep: FeatureDeps): Unit = afterAllFeature(dep)
    }
}
