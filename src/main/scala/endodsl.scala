package net.debasishg.snippet.endo

import java.util.{Date, Calendar}
import scalaz._
import Endo._
import WriterT._
import scalaz.syntax.all._
import scalaz.syntax.std.all._

object endodsl {
  case class Task(name: String)
  case class Project(name: String, startDate: java.util.Date, endDate: Option[java.util.Date] = None, 
                     tasks: List[Task] = List(), deps: List[(Task, Task)] = List())

  val withTask = (t: Task, p: Project) => p.copy(tasks = t :: p.tasks)
  val withDependency = (t: Task, on: Task, p: Project) => p.copy(deps = (t, on) :: p.deps)

  def task(n: String): Writer[Endo[Project], Task] = {
    val t = Task(n)
    for {
      _ <- tell(((p: Project) => withTask(t, p)).endo)
    } yield t
  }

  def dependsOn(t: Task, on: Task): Writer[Endo[Project], Task] = {
    for {
      _ <- tell(((p: Project) => withDependency(t, on, p)).endo)
    } yield t
  }

  def project(name: String, startDate: Date)(e: Writer[Endo[Project], Task]) = {
    val p = Project(name, startDate)
    e.run._1(p)
  }
}

object Main {
  import endodsl._
  val now = Calendar.getInstance.getTime
  val p = 
    project("xenos", now) {
      for {
        a <- task("study requirements")
        b <- task("do analysis")
        _ <- dependsOn(b, a)
        c <- task("design & code")
        _ <- dependsOn(c, b)
        d <- dependsOn(c, a)
      } yield d
    }
  println(p)
    
}
