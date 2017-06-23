package com.al333z.antitest.kernel

import com.al333z.antitest.TestBuilders.testSuite
import org.scalatest.FeatureSpec
import com.al333z.antitest.TestBuilders._
import com.al333z.antitest.TryInstances

import scala.util.{Success, Try}

class Example extends FeatureSpec with AntiTestDSL[Try] with FeatureRunner[Try] with TryInstances {

  runFeature[String] {

    testSuite[Try, String]("Prova")(
      beforeAll = {
        "String dep"
      },

      scenarios = Seq(
        test[Try, String]("scenario di prova")(

          scenario = stringDep => {

            val failPredicate = Predicate.lift[Errors, String](
              failure = Vector("This happens when predicate fail"),
              p = string => false
            )

            val successPredicate = Predicate.lift[Errors, String](
              failure = Vector("This happens when predicate fail"),
              p = string => false
            )

            assertP("Una prova di Predicate")("Pippo")(failPredicate.and(successPredicate))
          }
      )
      )
    )
  }
}
