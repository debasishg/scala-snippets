// from http://eed3si9n.com/learning-scalaz/Free+Monad.html
package net.debasishg.snippet.free

import scalaz._
import Scalaz._
import Free._

object toyfree {
  sealed trait Toy[A]
  case class Output[A, B](a: A, next: B) extends Toy[A]
  case class Bell[A, B](next: B) extends Toy[A]
  case class Done[A]() extends Toy[A]

  implicit def toyFunctor[X]: Functor[({type λ[+α] = Toy[X]})#λ] = new Functor[({type λ[+α] = Toy[X]})#λ] {
    def map[A, B](a: Toy[X])(f: A => B) = a match {
      case o: Output[X, A] => Output(o.a, f(o.next))
      case b: Bell[X, A] => Bell(f(b.next))
      case Done() => Done()
    }
  }

  def output[A](a: =>A) = liftF[({type λ[+α] = Toy[A]})#λ, Unit](Output(a, ()))
  def bell[A] = liftF[({type λ[+α] = Toy[A]})#λ, Unit](Bell(()))
  def done[A] = liftF[({type λ[+α] = Toy[A]})#λ, Unit](Done())

  def showProgram[A: Show, R: Show](p: Free[({type λ[+α] = Toy[A]})#λ, R]): String = p.resume.fold(
    { 
      case Output(a: A, next: Free[({type λ[+α] = Toy[A]})#λ, R]) =>
        "output " + Show[A].shows(a) + "\n" + showProgram(next)

      case Bell(next: Free[({type λ[+α] = Toy[A]})#λ, R]) =>
        "bell" + "\n" + showProgram(next)

      case d: Done[A] => "done\n"
    },
    {
      r: R => "return " + Show[R].shows(r) + "\n"
    }
  )

  /**
  scala> val subroutine = output('A')
  subroutine: scalaz.Free[[+?]net.debasishg.snippet.free.toyfree.Toy[Char],Unit] = Suspend(Output(A,Return(())))

  scala> val program = for {
       |   _ <- subroutine
       |   _ <- bell[Char]
       |   _ <- done[Char]
       | } yield ()
  program: scalaz.Free[[+?]net.debasishg.snippet.free.toyfree.Toy[Char],Unit] = Gosub(<function0>,<function1>)

  scala> showProgram(program)
  res3: String = 
  "outputA
  bell
  done
  "
  **/
}

