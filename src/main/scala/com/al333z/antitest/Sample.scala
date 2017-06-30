package com.al333z.antitest

import cats.instances.VectorInstances
import cats.instances.string._
import com.al333z.antitest.TestBuilders._
import com.al333z.antitest.kernel.{AntiTestDSL, FeatureRunner}

import scala.language.higherKinds
import scala.util.{Random, Success, Try}

class Sample extends FeatureRunner[Try] with AntiTestDSL[Try] with VectorInstances with TryInstances {

  runFeature[Int] {
    testSuite[Try, Int]("Sample description")(

      beforeAll = {
        42
      },

      scenarios = Seq(

        test[Try, Int, String]("Happy scenario")(
          before = { _ => "foo" },
          scenario =
            (intDep, strDep) =>
              for {
                x <- given("an int " + intDep)(Success(intDep))
                y <- and("a string " + strDep)(Success(strDep))
                _ <- when("i'm happy")(Success(()))
                _ <- assert("i succeed")(1 == 1)
                _ <- assertEventually("i succeed, eventually") { () => Random.nextInt(10) % 2 == 0 }
              } yield ()
        ),

        test[Try, Int]("Failing scenario")(
          scenario =
            intDep =>
              for {
                x <- given("an int")(Success(intDep))
                y <- and("a string")(Success("bar"))
                _ <- when("i'm moody")(Success(()))
                _ <- assertF("i succeed")(Success(true))
                _ <- assert("and then i fail")(1 == 2)
              } yield ()
        )
      )

        ++

        tests[Try, Int, String]("a test with samples")(
          scenario = (fd, sample) => {
            for {
              _ <- given("an int: " + fd)(Success(fd))
              _ <- assertEquals(sample + " == Alice")(sample, "Alice")
            } yield ()
          },
          examples = Seq("Bob", "Alice")
        )
    )
  }

}
