package com.al333z.antitest.kernel

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.semigroup._
import cats.syntax.validated._

sealed trait FailedAssertion[E] {
  val error: E
  val errorMessage: String
}

sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  val description: String

  def run(a: A)(implicit sem: Semigroup[E]): Validated[FailedAssertion[E], (String, E)] = {
    this match {
      case Pure(_, fun) => fun(a)
      case not: Not[E, A] => not.p.run(a) match {
        case Valid(r) => Invalid(new FailedAssertion[E] {
            override val error: E = r._2
            override val errorMessage: String = "not"
          })
        case Invalid(assertion) => Valid((not.description, assertion.error))
      }
      case and: And[E, A] => and.left.run(a) match {
        case Valid(l) => and.right.run(a) match {
          case Valid(r) => Valid(and.description, r._2 |+| r._2)
          case Invalid(er) => Invalid(new FailedAssertion[E] {
            override val error: E = er.error
            override val errorMessage: String = "and"
          })
        }
        case Invalid(l) => and.right.run(a) match {
          case Valid(r) => Invalid(new FailedAssertion[E] {
            override val error: E = l.error
            override val errorMessage: String = "and"
          })
          case Invalid(es) => Invalid(new FailedAssertion[E] {
            override val error: E = l.error |+| es.error
            override val errorMessage: String = "and"
          })
        }
      }
      case or: Or[E, A] => or.left.run(a) match {
        case Valid(l) => Valid(or.description, l._2)
        case Invalid(el) =>
          or.right.run(a) match {
            case Valid(r) => Valid(or.description, r._2)
            case Invalid(er) => Invalid(new FailedAssertion[E] {
              override val error: E = el.error |+| er.error
              override val errorMessage: String = "or"
            })
          }
      }
    }
  }
}

object Predicate {

  def not[E, A](predicate: Predicate[E, A]): Predicate[E, A] = Not(predicate)

  def apply[E, A](description: String)(f: A => Validated[FailedAssertion[E], (String, E)]) = Pure(description, f)

  def lift[E, A](description: String,
                 failedAssertions: E,
                 p: A => Boolean) =
    Pure(description, (a: A) => if (p(a)) (description, failedAssertions).valid
    else new FailedAssertion[E] {
      override val error: E = failedAssertions
      override val errorMessage: String = ""
    }.invalid)

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] {
    override val description: String = left.description + " and " + right.description
  }

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] {
    override val description: String = left.description + " or " + right.description
  }

  final case class Not[E, A](p: Predicate[E, A]) extends Predicate[E, A] {
    override val description: String = "not " + p.description
  }

  final case class Pure[E, A](desc: String, fun: A => Validated[FailedAssertion[E], (String, E)]) extends Predicate[E, A] {
    override val description: String = desc
  }
}
