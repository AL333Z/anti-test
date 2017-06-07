package com.al333z.antitest

import cats.functor.Contravariant
import cats.syntax.semigroup._
import cats.{Applicative, Apply, Functor, MonadError, Monoid, Semigroup, Show}

import scala.language.higherKinds

// A variation of WriterT (thanks cats) which let's you also append non-happy path logs
final case class LoggerT[F[_], L, V](run: F[(L, V)]) {
  def tell(l: L)(implicit functorF: Functor[F], semigroupL: Semigroup[L]): LoggerT[F, L, V] =
    mapWritten(_ |+| l)

  def written(implicit functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  def ap[Z](f: LoggerT[F, L, V => Z])(implicit F: Apply[F], L: Semigroup[L]): LoggerT[F, L, Z] =
    LoggerT(
      F.map2(f.run, run) {
        case ((l1, fvz), (l2, v)) => (L.combine(l1, l2), fvz(v))
      })

  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): LoggerT[F, L, Z] =
    LoggerT {
      functorF.map(run) { z => (z._1, fn(z._2)) }
    }

  def contramap[Z](fn: Z => V)(implicit F: Contravariant[F]): LoggerT[F, L, Z] =
    LoggerT {
      F.contramap(run) { z => (z._1, fn(z._2)) }
    }

  // leveraging a MonadError to combine also error logs
  def flatMap[U](f: V => LoggerT[F, L, U])(implicit
                                           monadErrorFL: MonadError[F, L],
                                           semigroupL: Semigroup[L]): LoggerT[F, L, U] =
    LoggerT {
      monadErrorFL.flatMap(run) { lv =>
        monadErrorFL.handleErrorWith(
          monadErrorFL.map(f(lv._2).run) { lv2 =>
            (semigroupL.combine(lv._1, lv2._1), lv2._2)
          }
        )(errorLog => monadErrorFL.raiseError(semigroupL.combine(lv._1, errorLog)))
      }
    }

  def mapBoth[M, U](f: (L, V) => (M, U))(implicit functorF: Functor[F]): LoggerT[F, M, U] =
    LoggerT {
      functorF.map(run)(f.tupled)
    }

  def bimap[M, U](f: L => M, g: V => U)(implicit functorF: Functor[F]): LoggerT[F, M, U] =
    mapBoth((l, v) => (f(l), g(v)))

  def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): LoggerT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

  def swap(implicit functorF: Functor[F]): LoggerT[F, V, L] =
    mapBoth((l, v) => (v, l))

  def reset(implicit monoidL: Monoid[L], functorF: Functor[F]): LoggerT[F, L, V] =
    mapWritten(_ => monoidL.empty)

  def show(implicit F: Show[F[(L, V)]]): String = F.show(run)
}

object LoggerT {

  def lift[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): LoggerT[F, L, V] =
    LoggerT(F.map(fv)(v => (monoidL.empty, v)))

}
