package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def traverse[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def traverse[A](t: Sequence[A])(f: A => Unit): Unit = t match
      case Cons(head, tail) => f(head); traverse(tail)(f)
      case _ => ()

  given Traversable[Optional] with
    def traverse[A](t: Optional[A])(f: A => Unit): Unit = t match
      case Just(head) => f(head)
      case _ => ()

  def logAll[A, T[_]](t: T[A])(f: A => Unit)(using traversable: Traversable[T]): Unit =
    traversable.traverse(t)(f)

@main def main =
  import u04lab.Ex5Traversable.logAll
  
  val si = Cons(10, Cons(20, Cons(30, Nil())))
  def log(a: Int): Unit = println("The next value is: " + a)
  println:
    logAll(si)(log)
 


  
