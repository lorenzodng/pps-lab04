package tasks.adts

import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  //esempio di utilizzo di trait con Optional
  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A]

    extension [A](stack: Stack[A])

      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:

    type Stack[A] = Sequence[A]

    def empty[A]: Stack[A] = Sequence.Nil()

    extension [A](stack: Stack[A])

      def push(a: A): Stack[A] = Sequence.Cons(a, stack)

      def pop(): Optional[(A, Stack[A])] = stack match
        case Sequence.Cons(head, tail) => Optional.Just(head, tail) //Optional.Just è utilizzato per fornire una rappresentazione dello stack, tale che: l'elemento testa è quello rimosso e l'elemento coda è il contenuto rimanente dello stack. Anche se Just è definito come Just(a: A) è possibile rappresentare A come una tupla
        case _ => Optional.Empty()

      def asSequence(): Sequence[A] = stack