package com.al333z.antitest.kernel

import scala.language.higherKinds

trait Feature[F[_], FeatureDeps] {
  val description: String

  def scenarios: Seq[Scenario[F, FeatureDeps]]

  def beforeAll: FeatureDeps

  def afterAll(dep: FeatureDeps): Unit = ()
}
