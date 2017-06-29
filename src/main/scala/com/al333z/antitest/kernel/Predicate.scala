package com.al333z.antitest.kernel

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.semigroup._
import cats.syntax.validated._
import com.al333z.antitest.kernel.ErrorBool.{AndError, NotError, OrError, PureError}
import com.al333z.antitest.kernel.ValidBool.{AndValid, NotValid, OrValid, PureValid}

sealed trait ErrorStructure[E] {
  val error: E
  val description: String
}

sealed trait ValidStructure[E] {
  val validPredicate: E
  val description: String
}
object ValidBool {
  case class AndValid[E](e: E) extends ValidStructure[E] {
    override val validPredicate: E = e
    override val description: String = "and"
  }

  case class OrValid[E](e: E) extends ValidStructure[E] {
    override val validPredicate: E = e
    override val description: String = "or"
  }

  case class NotValid[E](e: E) extends ValidStructure[E] {
    override val validPredicate: E = e
    override val description: String = "not"
  }

  case class PureValid[E](e: E) extends ValidStructure[E] {
    override val validPredicate: E = e
    override val description: String = ""
  }
}
object ErrorBool {

  case class AndError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "and"
  }

  case class OrError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "or"
  }

  case class NotError[E](e: E) extends ErrorStructure[E] {
    override val error: E = e
    override val description: String = "not"
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

  def run(a: A)(implicit sem: Semigroup[E]): Validated[ErrorStructure[E], ValidStructure[E]] = {
    this match {
      case Pure(fun) => fun(a)
      case Not(p) => p.run(a) match {
        case Valid(e) => Invalid(NotError(e.validPredicate))
        case Invalid(es) => Valid(NotValid(es.error))
      }
      case And(lp, rp) => lp.run(a) match {
        case Valid(l) => rp.run(a) match {
          case Valid(r) => Valid(AndValid(l.validPredicate |+| r.validPredicate))
          case Invalid(er) => Invalid(AndError(er.error))
        }
        case Invalid(l) => rp.run(a) match {
          case Valid(r) => Invalid(AndError(l.error))
          case Invalid(es) => Invalid(AndError(l.error |+| es.error))
        }
      }
      case Or(lp, rp) => lp.run(a) match {
        case Valid(l) => Valid(OrValid(l.validPredicate))
        case Invalid(el) =>
          rp.run(a) match {
            case Valid(r) => Valid(OrValid(r.validPredicate))
            case Invalid(er) => Invalid(OrError(el.error |+| er.error))
          }
      }
    }
  }
}

object Predicate {

  def not[E, A](predicate: Predicate[E, A]): Predicate[E, A] = Not(predicate)

  def apply[E, A](f: A => Validated[ErrorStructure[E], ValidStructure[E]]) = Pure(f)

  def lift[E, A](failure: E, p: A => Boolean) = Pure((a: A) => if (p(a)) PureValid(failure).valid else PureError(failure).invalid)

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Not[E, A](p: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](fun: A => Validated[ErrorStructure[E], ValidStructure[E]]) extends Predicate[E, A]

}