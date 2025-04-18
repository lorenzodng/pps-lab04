package u04lab
import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

/*  Exercise 4: 
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

//esempio di utilizzo di trait con given
object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  //definisco un'implementazione personalizzata del trait per int
  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  //definisco un'implementazione personalizzata del trait per double
  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0

  //definisco un'implementazione personalizzata del trait per string
  given Summable[String] with
     def sum(a1: String, a2: String): String = a1 + a2
     def zero: String = ""

  //metodo di esecuzione
  def sumAll[A: Summable](seq: Sequence[A])(using summable: Summable[A]) = //using è utilizzato per ottenere l'istanza dei Summable implementati con given
    def sumLoop(acc: A, seq: Sequence[A]): A = seq match
      case Nil() => acc
      case Cons(head, tail) => summable.sum(summable.sum(head, acc), sumLoop(acc, tail))  //ogni volta che viene richiamato questo metodo, a seconda del tipo di sequenza passata come parametro, viene eseguita una delle implementazioni del trait
    
    sumLoop(summable.zero, seq) //.zero definisce automaticamente il valore 0 (per int o double) o "" (per string), poprio perchè è privo di implementazione all'interno del trait

@main def trySummables =
  import u04lab.Ex4Summables.{sumAll, sumAllInt}

  val si = Cons(10, Cons(20, Cons(30, Nil())))
  println:
    sumAllInt(si) // 60

  println:
    sumAll(si) // 60

  val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
  println:
    sumAll(sd) // 60.0

  val ss = Cons("10", Cons("20", Cons("30", Nil())))
  println:
    sumAll(ss) // "102030"

