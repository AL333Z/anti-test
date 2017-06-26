package com.al333z.antitest.kernel

import com.al333z.antitest.TestBuilders.{testSuite, _}
import com.al333z.antitest.TryInstances
import com.al333z.antitest.kernel.Predicate._
import org.scalatest.FeatureSpec

import scala.util.Try

class Example extends FeatureSpec with AntiTestDSL[Try] with FeatureRunner[Try] with TryInstances {

  runFeature[String] {

    testSuite[Try, String]("Prova")(
      beforeAll = {
        "String dep"
      },

      scenarios = Seq(
        test[Try, String]("scenario di prova")(

          scenario = stringDep => {

            val failPredicate = predicate(description = "This happens when predicate fail")((_: String) => false)

            val notFailPredicate = not(predicate("This happens when predicate fail 2")((_: String) => false))

            assertP("Pippo")(failPredicate.and(notFailPredicate))
          }
        )
      )
    )
  }
}
