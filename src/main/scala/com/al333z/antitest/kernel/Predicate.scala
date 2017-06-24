package com.al333z.antitest.kernel
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import cats.syntax.cartesian._
import cats.syntax.semigroup._

sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def not: Predicate[E, A] = Not(this)


  def run(a: A)(implicit sem: Semigroup[E]): Validated[E, (Boolean, E)] = {
    this match {
      case Pure(fun) => fun(a)
      case Not(p) => p.run(a) match {
        case Valid((res,e)) => Invalid(e)
        case Invalid(e) => Valid((true,e))
      }
      case And(lp,rp) => (lp.run(a) |@| rp.run(a)).map( (rl,rr) => (rl._1 && rr._1, rl._2 |+| rr._2))
      case Or(lp,rp) => lp.run(a) match {
        case Valid(x) => Valid(x)
        case Invalid(e1) => {
          rp.run(a) match {
            case Valid(x) => Valid(x)
            case Invalid(e2) => Invalid(e1 |+| e2)
          }
        }
      }
    }
  }
}

object Predicate {

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Not[E, A](p: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](fun: A => Validated[E,(Boolean, E)]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, (Boolean, E)]) = Pure(f)

  def lift[E, A](failure: E, p: A => Boolean) = Pure((a: A) => if(p(a)) (true, failure).valid else failure.invalid)
}

