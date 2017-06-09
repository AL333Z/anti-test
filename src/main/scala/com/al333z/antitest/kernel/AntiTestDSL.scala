package com.al333z.antitest.kernel

import cats.MonadError
import cats.implicits._
import com.al333z.antitest.LoggerT

import scala.language.higherKinds

trait AntiTestDSL[F[_]] {

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

  def assertEventually(description: String)(assertion: () => Boolean)(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], Unit] = {

    def retry(assertion: () => Boolean, currentRetry: Int): LoggerT[F, Vector[String], Unit] = {
      if (assertion()) {
        val msg = "Then " + description + (if (currentRetry > 0) " (after " + currentRetry + " attempts)" else "")
        LoggerT[F, Vector[String], Unit](monadError.pure((Vector(msg), ())))
      }
      else if (currentRetry < 5) {
        Thread.sleep(500) // FIXME works, but ugly
        retry(assertion, currentRetry + 1)
      }
      else
        LoggerT.lift[F, Vector[String], Unit](
          monadError.raiseError(Vector("Assertion failed (after " + currentRetry + " attempts): " + description))
        )
    }

    retry(assertion, 0)
  }

  private def logged[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] = {
    for {
      _ <- LoggerT[F, Vector[String], Unit](monadError.pure((Vector(description), ())))
      res <- LoggerT.lift[F, Vector[String], A](task)
    } yield res
  }

}
