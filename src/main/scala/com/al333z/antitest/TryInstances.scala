package com.al333z.antitest

import cats.{Comonad, MonadError}

import scala.util.{Failure, Success, Try}

trait TryInstances {
  implicit val m: MonadError[Try, Vector[String]] with Comonad[Try] =
    new MonadError[Try, Vector[String]] with Comonad[Try] {
      override def flatMap[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: (A) => Try[Either[A, B]]): Try[B] =
        f(a) match {
          case Failure(t) => Failure(t)
          case Success(Left(b1)) => tailRecM(b1)(f)
          case Success(Right(c)) => Success(c)
        }

      override def raiseError[A](e: Vector[String]): Try[A] = Failure(new Throwable(e.mkString("\n")))

      // not sure this is really usefull except for logging use cases
      override def handleErrorWith[A](fa: Try[A])(f: (Vector[String]) => Try[A]): Try[A] = fa.recoverWith {
        case t => f(Vector(t.getMessage))
      }

      override def pure[A](x: A): Try[A] = Success(x)

      override def extract[A](x: Try[A]): A = x.get

      override def coflatMap[A, B](ta: Try[A])(f: Try[A] => B): Try[B] = Try(f(ta))
    }

}
