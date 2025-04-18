package tasks.monads

import u04.monads.Monads.Monad

/**
  * Exercise 6: 
    This module contains the implementation of a Try monad, which is a monad that 
    represents a computation that may fail. 
    Try to follow these steps:
    - Look at the implementation of Try, that is similar to the one of Optional
    - Try go define the Monad instance for Try
      - flatMap should consider only the Success case
      - in case of Failure, it should return the exception (fail fast)
    - Verify that the main works as expected
  */

//esempio di utilizzo di una monade
object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)

  //per definire una monade, è necessario introdurre due metodi principali: unit e flatMap, utilizzati per il for-yield
  given Monad[Try] with

    //inserisce un valore nella monade (ovvero tra for e yield)
    override def unit[A](value: A): Try[A] = TryImpl.Success(value)

    extension [A](m: Try[A])

      //gestisce l'esecuzione di un'operazione richiamata sugli elementi della monade
      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value) => f(value) //esegue la funzione
        case TryImpl.Failure(exception) => exception.printStackTrace(); TryImpl.Failure(exception) //stampo un'eccezione

  extension [A](m: Try[A])

    //metodo utile per la verifica dell'esecuzione della monade tramite assert (non strettamente utile in questo contesto)
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other


@main def main: Unit =
  import Ex6TryModel.*

  val result = for
    a <- success(10)
    b <- success(30)
  yield a + b

  assert(result.getOrElse(-1) == 40)

  val result2 = for 
    a <- success(10)
    b <- failure(new RuntimeException("error"))
    c <- success(30)
  yield a + c

  assert(success(20).map(_ + 10).getOrElse(-1) == 30)
  assert(result2.getOrElse(-1) == -1)
