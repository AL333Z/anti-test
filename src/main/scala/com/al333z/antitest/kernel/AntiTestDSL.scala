package com.al333z.antitest.kernel

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import cats.{Eq, MonadError}
import com.al333z.antitest.LoggerT
import scala.concurrent.duration.{Duration, _}
import scala.language.{higherKinds, postfixOps}

trait AntiTestDSL[F[_]] {

  type Errors = Vector[String]

  def predicate[A](description: String)(pred: A => Boolean): Predicate[Errors, A] =
    Predicate.lift[Errors, A](description ,Vector(description), pred)

  def given[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, A] =
    logged("Given " + description)(task)

  def and[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, A] =
    logged("And " + description)(task)

  def when[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, A] =
    logged("When " + description)(task)

  def assert(description: String)(assertion: Boolean)(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {
    if (assertion) LoggerT[F, Errors, Unit](monadError.pure((Vector("Then " + description), ())))
    else LoggerT.lift[F, Errors, Unit](monadError.raiseError(Vector("Assertion failed: " + description)))
  }

  def assertP[A](a: A)(predicate: Predicate[Errors, A])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {

    predicate.run(a) match {
      case Valid(_) => LoggerT[F, Errors, Unit](monadError.pure((Vector("Then " + predicate.description), ())))
      case Invalid(e) =>
        LoggerT.lift[F, Errors, Unit](monadError.raiseError(
          Vector("Predicate failed: \n" + e.error.mkString("\n" + e.errorMessage + " \n"))
        ))
    }
  }

  def assertEventuallyFP[A](fa: F[A])(predicate: Predicate[Errors, A], maxRetry: Int = 5, delay: Duration = 500 millis)(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {

    def retry(f: F[A], currentRetry: Int, delay: Duration): F[(Errors, Unit)] = {
      f.flatMap {
        a =>
          predicate.run(a) match {
            case Valid(message) =>
              monadError.pure((Vector("Then " + predicate.description), ()))
            case Invalid(e) =>
              if (currentRetry < maxRetry) {
                Thread.sleep(delay.toMillis) // FIXME works, but ugly
                retry(fa, currentRetry + 1, delay)
              } else
                monadError.raiseError(
                  Vector("Predicate failed: \n" + e.error.mkString("\n" + e.errorMessage + " \n"))
                )
          }
      }
    }

    LoggerT[F, Errors, Unit](
      retry(fa, 0, delay).handleErrorWith {
        err => {
          monadError.raiseError[(Errors, Unit)](Vector("Assertion failed with exception:\n" + err.mkString("\n")))
        }
      }
    )
  }

  def assertF(description: String)(assertion: F[Boolean])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] =
    assertEventuallyF(description)(assertion, 1)

  def assertEquals[A](description: String)(expected: A, actual: A)(
    implicit monadError: MonadError[F, Errors], eq: Eq[A]): LoggerT[F, Errors, Unit] = {
    if (eq.eqv(expected, actual)) LoggerT[F, Errors, Unit](monadError.pure((Vector("Then " + description), ())))
    else LoggerT.lift[F, Errors, Unit](monadError.raiseError(Vector("Assertion failed: expected -> " + expected + " found -> " + actual)))
  }

  def assertEqualsEventually[A](description: String)(v1: => A, v2: => A, maxRetry: Int = 5, delay: Duration = 1500 millis)(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {

    def retry(assertion: () => Boolean, currentRetry: Int): LoggerT[F, Errors, Unit] = {
      print("\n\n\nretrying...\n\n\n")
      if (assertion()) {
        val msg = "Then " + description + (if (currentRetry > 0) " (after " + currentRetry + " attempts)" else "")
        LoggerT[F, Errors, Unit](monadError.pure((Vector(msg), ())))
      }
      else if (currentRetry < 5) {
        Thread.sleep(delay.toMillis)
        retry(assertion, currentRetry + 1)
      }
      else
        LoggerT.lift[F, Errors, Unit](
          monadError.raiseError(Vector("Assertion failed (after " + currentRetry + " attempts): " +
            description + "\n\n  Expected: " + v1 + ", but found: " + v2 + "\n"))
        )
    }
    val assertion: () => Boolean = () => v1.equals(v2)

    retry(assertion, 0)
  }

  def assertEventually(description: String)(assertion: () => Boolean, maxRetry: Int = 5, delay: Duration = 500 millis)(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {

    def retry(assertion: () => Boolean, currentRetry: Int): LoggerT[F, Errors, Unit] = {
      if (assertion()) {
        val msg = "Then " + description + (if (currentRetry > 0) " (after " + currentRetry + " attempts)" else "")
        LoggerT[F, Errors, Unit](monadError.pure((Vector(msg), ())))
      }
      else if (currentRetry < 5) {
        Thread.sleep(delay.toMillis) // FIXME works, but ugly
        retry(assertion, currentRetry + 1)
      }
      else
        LoggerT.lift[F, Errors, Unit](
          monadError.raiseError(Vector("Assertion failed (after " + currentRetry + " attempts): " + description))
        )
    }
    retry(assertion, 0)
  }

  def assertEventuallyF(description: String)(assertion: F[Boolean], maxRetry: Int = 5, delay: Duration = 500 millis)(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, Unit] = {

    def retry(assertion: F[Boolean], currentRetry: Int, delay: Duration): F[(Errors, Unit)] = {
      assertion.flatMap { predicate: Boolean ⇒
        if (predicate) monadError.pure((Vector("Then " + description), ()))
        else if (currentRetry < maxRetry) {
          Thread.sleep(delay.toMillis) // FIXME works, but ugly
          retry(assertion, currentRetry + 1, delay)
        }
        else
          monadError.raiseError[(Errors, Unit)](Vector("Assertion failed (after " + currentRetry + " attempts): " + description))
      }
    }

    LoggerT[F, Errors, Unit](
      retry(assertion, 0, delay).handleErrorWith {
        err => {
          monadError.raiseError[(Errors, Unit)](Vector("Assertion failed with exception: " + description + "\n" + err.mkString("\n")))
        }
      }
    )
  }

  private def logged[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Errors]): LoggerT[F, Errors, A] = {
    for {
      _ <- LoggerT[F, Errors, Unit](monadError.pure((Vector(description), ())))
      res <- LoggerT.lift[F, Errors, A](task)
    } yield res
  }
}
