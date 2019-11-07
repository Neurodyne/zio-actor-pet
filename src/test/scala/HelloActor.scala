package hello

import zio.console.{ putStrLn }
import zio.actors.Actor.Stateful
import zio.{ IO, Ref, Schedule }
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

import zio.actors.{ Actor, Supervisor }
import zio.test.environment.Live

import helper._

object CounterUtils {
  sealed trait Message[+_]
  case object Reset    extends Message[Unit]
  case object Increase extends Message[Unit]
  case object Get      extends Message[Int]
}

object TickUtils {
  sealed trait Message[+_]
  case object Tick extends Message[Unit]
}

object ActorsSpec
    extends DefaultRunnableSpec(
      suite("Test the basic actor behavior")(
        testM("sequential message processing") {

          import CounterUtils._

          for {
            actor <- Actor.stateful(Supervisor.none)(0)(handler)
            _     <- actor ! Increase
            _     <- actor ! Increase
            c1    <- actor ! Get
            _     <- actor ! Reset
            c2    <- actor ! Get
          } yield assert(c1, equalTo(2)) && assert(c2, equalTo(0))
        },
        testM("Print hello") {

          for {
            _ <- Live.live(putStrLn(msg))

          } yield assert(0, equalTo(0))
        },
        testM("Actor says hello") {
          for {
            actor <- Actor.stateful(Supervisor.none)(0)(strHandler)
            out   <- actor ! sayHello
          } yield assert(out, equalTo(msg))

        } //@@ skip
      )
    )

object helper {
  val msg        = "hello"
  def sayHello() = msg

  import CounterUtils._
  type Id[A] = A
  type AString[A] = String

  val handler = new Stateful[Int, Nothing, Message] {
    override def receive[A](state: Int, msg: Message[A]): IO[Nothing, (Int, A)] =
      msg match {
        case Reset    => IO.effectTotal((0, ()))
        case Increase => IO.effectTotal((state + 1, ()))
        case Get      => IO.effectTotal((state, state))
      }
  }

  val strHandler = new Stateful[Int, Nothing, Message] {
    override def receive[A](state:Int, msg: AString[A]) = IO.effectTotal((state, msg))
  }
}
