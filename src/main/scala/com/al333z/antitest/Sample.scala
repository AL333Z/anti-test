package com.al333z.antitest

import cats.instances.VectorInstances
import com.al333z.antitest.TestBuilders._
import com.al333z.antitest.kernel.{AntiTestDSL, FeatureRunner}

import scala.language.higherKinds
import scala.util.{Success, Try}

class Sample extends FeatureRunner[Try] with AntiTestDSL[Try] with VectorInstances with TryInstances {

  runFeature[Int] {

    testSuite("Sample description")(42)(

      test[Try, Int, String]("Happy scenario")(_ => "foo") {
        (intDep, strDep) =>
          for {
            x <- given("an int")(Success(42))
            y <- and("a string")(Success("bar"))
            _ <- when("i'm happy")(Success())
            _ <- assert("i succeed")(1 == 1)
          } yield ()
      },

      test[Try, Int, String]("Failing scenario")(_ => "bar") {
        (intDep, strDep) =>
          for {
            x <- given("an int")(Success(42))
            y <- and("a string")(Success("bar"))
            _ <- when("i'm grumpy")(Success())
            _ <- assert("i fail")(1 == 2)
          } yield ()
      }
    )

  }

}
