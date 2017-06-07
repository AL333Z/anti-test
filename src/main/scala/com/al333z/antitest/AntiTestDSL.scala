package com.al333z.antitest

import cats.MonadError
import cats.implicits._

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

  private def logged[A](description: String)(task: F[A])(
    implicit monadError: MonadError[F, Vector[String]]): LoggerT[F, Vector[String], A] = {
    for {
      _ <- LoggerT[F, Vector[String], Unit](monadError.pure((Vector(description), ())))
      res <- LoggerT.lift[F, Vector[String], A](task)
    } yield res
  }

}
