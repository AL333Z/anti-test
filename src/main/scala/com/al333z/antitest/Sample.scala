package com.al333z.antitest

import cats.instances.VectorInstances

import scala.util.{Success, Try}

// importing required instances..
object Sample extends AntiTestDSL[Try] with VectorInstances with TryInstances with App {

  val failingScenario: LoggerT[Try, Vector[String], Unit] = for {
    x <- given("an int")(Success(42))
    y <- and("a string")(Success("bar"))
    _ <- when("i'm grumpy")(Success())
    _ <- assert("i fail")(1 == 2)
  } yield ()

  val happyScenario: LoggerT[Try, Vector[String], Unit] = for {
    x <- given("an int")(Success(42))
    y <- and("a string")(Success("bar"))
    _ <- when("i'm happy")(Success())
    _ <- assert("i succeed")(1 == 1)
  } yield ()

  printRes(failingScenario.run)
  printRes(happyScenario.run)

  def printRes(t: Try[(Vector[String], Unit)]): Unit = t.fold(
    t => println(t.getMessage + "\nFAILED\n"),
    res => println(res._1.mkString("\n") + "\nPASSED\n")
  )

}
