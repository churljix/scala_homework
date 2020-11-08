import cats._
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

object CatsHw10 extends App {
  /**
   * Cats identity - Id
   * Looks a bit pointless, but at the right time and place can be very useful
   **/

  /**
   * Cats is all about using Monads tu improve the effectiveness of code.
   * In Monad[F[_]], F represents an “effect” like Future , Either or Option
   * that allows applying a function (A => B or A => F[B]) to a single effectful value (A)
   * without needing to “leave” that “effect” (F) and
   * convert that value to desire effectful value (B).
   *
   * Example of a function using Monad:
   */
    def stringConcat[F[_]: Monad](string1: F[String], string2: F[String]): F[String] = for {
      x <- string1
      y <- string2
    } yield s"$x $y"

  /**
   * To call the function the strings must be wrapped into Moands - Option, Either, Future
   * but it won't work with just String values
   **/
    println(stringConcat(Option("Hello"), Option("World!"))) //good Some(Hello World!)
  /**
   * Function call using Strings wrapped in monads returns a monad - Some(Hello World!)
   * When the same function is called with plain Strings it crashes with error:
   * Could not find an instance of Monad for Comparable
   */
    println(stringConcat("Hello", "World!")) //bad

  /**
   * This is where Id comes in play.
   * Id is an Identity monad also known as Identity functor, or Identity monad.
   * The Identity monad definition is very simple:
   */
    type Id[A] = A

  /**
   * It's a type alias that turns a type into a single-parameter type constructor.
   * Any value of any type can be casted using Id.
   * Id monad allows to write functions that work with monadic and non-monadic values.
   *
   * In this example Id can be used in two ways:
   * adding type for each parameter or adding [Id] before the parameters
   * Both of these calls will print out the same "Hello World2!" String.
   */
  println(stringConcat("Hello": Id[String], "World2!": Id[String]))
  println(stringConcat[Id]("Hello", "World2!"))

  /**
   * Id can be used together with Functor and Monad,
   * which means that map, flatMap and pure functions can be used with it.   *
   **/

  val pureId = Monad[Id].pure(3)
  println(pureId)
  val flatMapId = Monad[Id].flatMap(pureId)(_ + 1)
  println(flatMapId)

  for {
    x <- pureId
    y <- flatMapId
  } yield println(x + y)

  /**
   * A real life situation where Id can be useful:
   * Code can be run asynchronously in the production using the Future and
   * synchronously in the test using the Identity Monad and
   * easily switch between asynchronous and synchronous world.
   */

  /**
   * Sources:
   * https://typelevel.org/cats/datatypes/id.html
   * https://eed3si9n.com/herding-cats/Id.html
   * https://itnext.io/benefits-of-identity-monad-in-scala-cats-a2cb0baef639
   * "Scala with Cats", https://underscore.io/books/scala-with-cats/
   */
}
