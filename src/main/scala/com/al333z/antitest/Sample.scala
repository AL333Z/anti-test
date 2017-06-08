package com.al333z.antitest

import cats.instances.VectorInstances
import com.al333z.antitest.kernel.{AntiTestDSL, Feature, FeatureRunner, Scenario}
import org.scalatest.FeatureSpec

import scala.util.{Success, Try}

// importing required instances..
class Sample extends FeatureSpec with FeatureRunner[Try] with AntiTestDSL[Try] with VectorInstances with TryInstances {

  val happyScenario: LoggerT[Try, Vector[String], Unit] = for {
    x <- given("an int")(Success(42))
    y <- and("a string")(Success("bar"))
    _ <- when("i'm happy")(Success())
    _ <- assert("i succeed")(1 == 1)
  } yield ()

  val failingScenario: LoggerT[Try, Vector[String], Unit] = for {
    x <- given("an int")(Success(42))
    y <- and("a string")(Success("bar"))
    _ <- when("i'm grumpy")(Success())
    _ <- assert("i fail")(1 == 2)
  } yield ()

  // TODO better DSL to create features and scenarios
  runFeature[Int, Vector[String]] {
    new Feature[Try, Int] {
      override val description: String = "Sample description"

      override def scenarios: Seq[Scenario[Try, Int]] = Seq(
        new Scenario[Try, Int] {
          override type ScenarioDeps = String

          override val description: String = "Happy scenario"

          override def before(beforeAll: Int): ScenarioDeps = "foo"

          override def behaviour(beforeAll: Int, before: String): LoggerT[Try, Vector[String], Unit] = happyScenario
        },
        new Scenario[Try, Int] {
          override type ScenarioDeps = String

          override val description: String = "Failing scenario"

          override def before(beforeAll: Int): ScenarioDeps = "bar"

          override def behaviour(beforeAll: Int, before: String): LoggerT[Try, Vector[String], Unit] = failingScenario
        }
      )

      override def beforeAll: Int = 42
    }
  }

}
