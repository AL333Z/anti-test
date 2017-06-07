What's this?
===

A POC about BDD, leveraging Functional Programming techniques.

Motivation
===

In the past we saw (and wrote) really bad code which used cucumber, with tons of global variables, null references, runtime surprises, etc..

This is our attempt to redeem ourselves.

Authors
===

- [@azanin](https://github.com/azanin)

- [@al333z](https://github.com/AL333Z)


How
===

The idea is to offer a DSL which is both lawful and readable.

```scala
  val failingScenario = for {
    x <- given("an int")(42)
    y <- and("a string")("bar")
    _ <- when("i'm grumpy")(())
    _ <- assert("i fail")(1 == 2)
  } yield ()

  val happyScenario = for {
    x <- given("an int")(42)
    y <- and("a string")("bar")
    _ <- when("i'm happy")(())
    _ <- assert("i succeed")(1 == 1)
  } yield ()
```

Looks like a `Writer`, but we need to also run effects `F[_]` on each step.
So let's move to `WriterT`, and it's good. Except for failing scenarios, in which we lose the logs.

Let's introduce `LoggerT`, a WriterT which preserve and combine error logs.
How?

```scala
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
```

Compromise.

Putting a `MonadError` constraint instead of just `FlatMap`, and forcing error to have type `L`.
With a `MonadError[F, L]` in scope, we can combine logs with the current failure.

All the remaining parts are only about offering a nice syntax to the DSL.

Feedback required
===

The code is really minimal, but effective.
Any feedback is more than appreaciated :)
