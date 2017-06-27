package com.al333z.antitest.kernel

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.cartesian._
import cats.syntax.semigroup._
import cats.syntax.validated._
import com.al333z.antitest.kernel.A.{AndError, NotError, OrError, PureError}


sealed trait ErrorStructure[E] {
  val error: E
  val description: String
}

object A {
  case class AndError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "AND"
  }
  case class OrError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "OR"
  }
  case class NotError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "NOT"
  }
  case class PureError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = ""
  }
}


sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def run(a: A)(implicit sem: Semigroup[E]): Validated[ErrorStructure[E], (Boolean, E)] = {
    this match {
      case Pure(fun) => fun(a)
      case Not(p) => p.run(a) match {
        case Valid((res, e)) => Invalid(NotError(e))
        case Invalid(es) => Valid((true, es.error))
      }
      case And(lp, rp) => lp.run(a) match {
        case Valid(l) => rp.run(a) match {
          case Valid(r) => Valid(true,l._2 |+| r._2 )
          case Invalid(es) => Invalid(AndError(es.error))
        }
        case Invalid(l) => rp.run(a) match {
          case Valid(r) => Invalid(AndError(l.error))
          case Invalid(es) => Invalid(AndError(l.error |+| es.error))
        }
      }
      case Or(lp, rp) => lp.run(a) match {
        case Valid(x) => Valid(x)
        case Invalid(e1) => {
          rp.run(a) match {
            case Valid(x) => Valid(x)
            case Invalid(e2) => Invalid(OrError(e1.error |+| e2.error))
          }
        }
      }
    }
  }
}

object Predicate {

  def not[E, A](predicate: Predicate[E, A]): Predicate[E, A] = Not(predicate)

  def apply[E, A](f: A => Validated[ErrorStructure[E], (Boolean, E)]) = Pure(f)

  def lift[E, A](failure: E, p: A => Boolean) = Pure((a: A) => if (p(a)) (true, failure).valid else PureError(failure).invalid)

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Not[E, A](p: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](fun: A => Validated[ErrorStructure[E], (Boolean, E)]) extends Predicate[E, A]
}


