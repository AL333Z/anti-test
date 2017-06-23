package com.al333z.antitest.kernel

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import cats.{Eq, MonadError}
import com.al333z.antitest.LoggerT

import scala.concurrent.duration.{Duration, _}
import scala.language.{higherKinds, postfixOps}

trait AntiTestDSL[F[_]] {

  type Errors = Vector[String]

  def given[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] =
    logged("Given " + description)(task)

  def and[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] =
    logged("And " + description)(task)

  def when[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] =
    logged("When " + description)(task)

  def assert(description: String)(assertion: Boolean)(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] = {
    if (assertion) LoggerT[F, Vector[String], Unit](monadError.pure((Vector("Then " + description), ())))
    else LoggerT.lift[F, Vector[String], Unit](monadError.raiseError(Vector("Assertion failed: " + description)))
  }

  def assertP[A](description: String)(a: A)(predicate: Predicate[Errors, A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] = {
    predicate.run(a) match {
      case Valid(_) => LoggerT[F, Vector[String], Unit](monadError.pure((Vector("Then " + description), ())))
      case Invalid(e) =>
        LoggerT.lift[F, Vector[String], Unit](monadError.raiseError(
          Vector("Assertion failed: " + description) ++ e
        ))
    }
  }

  def assertF(description: String)(assertion: F[Boolean])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] =
    assertEventuallyF(description)(assertion, 1)

  def assertEquals[A](description: String)(expected: A, actual: A)(
    implicit monadError: MonadError[F, Vector[String]], eq: Eq[A]): LoggerT[F, Vector[String], Unit] = {
    if (eq.eqv(expected, actual)) LoggerT[F, Vector[String], Unit](monadError.pure((Vector("Then " + description), ())))
    else LoggerT.lift[F, Vector[String], Unit](monadError.raiseError(Vector("Assertion failed: expected -> " + expected + " found -> " + actual)))
  }

  def assertEventually(description: String)(assertion: () => Boolean, maxRetry: Int = 5, delay: Duration = 500 millis)(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] = {

    def retry(assertion: () => Boolean, currentRetry: Int): LoggerT[F, Vector[String], Unit] = {
      if (assertion()) {
        val msg = "Then " + description + (if (currentRetry > 0) " (after " + currentRetry + " attempts)" else "")
        LoggerT[F, Vector[String], Unit](monadError.pure((Vector(msg), ())))
      }
      else if (currentRetry < 5) {
        Thread.sleep(delay.toMillis) // FIXME works, but ugly
        retry(assertion, currentRetry + 1)
      }
      else
        LoggerT.lift[F, Vector[String], Unit](
          monadError.raiseError(Vector("Assertion failed (after " + currentRetry + " attempts): " + description))
        )
    }

    retry(assertion, 0)
  }

  def assertEventuallyF(description: String)(assertion: F[Boolean], maxRetry: Int = 5, delay: Duration = 500 millis)(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] = {

    def retry(assertion: F[Boolean], currentRetry: Int, delay: Duration): F[(Vector[String], Unit)] = {
      assertion.flatMap { predicate: Boolean ⇒
        if (predicate) monadError.pure((Vector("Then " + description), ()))
        else if (currentRetry < maxRetry) {
          Thread.sleep(delay.toMillis) // FIXME works, but ugly
          retry(assertion, currentRetry + 1, delay)
        }
        else
          monadError.raiseError[(Vector[String], Unit)](Vector("Assertion failed (after " + currentRetry + " attempts): " + description))
      }
    }

    LoggerT[F, Vector[String], Unit](
      retry(assertion, 0, delay).handleErrorWith {
        err => {
          monadError.raiseError[(Vector[String], Unit)](Vector("Assertion failed with exception: " + description + "\n" + err.mkString("\n")))
        }
      }
    )
  }

  private def logged[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] = {
    for {
      _ <- LoggerT[F, Vector[String], Unit](monadError.pure((Vector(description), ())))
      res <- LoggerT.lift[F, Vector[String], A](task)
    } yield res
  }

}
