import cats.data.EitherT

import cats.instances.future.*
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz" -> 6,
  "Bumblebee" -> 8,
  "Hot Rod" -> 10
)

def getPowerLevel(autobot: String): Response[Int] =
  powerLevels.get(autobot) match
    case Some(level) => EitherT.right(Future(level))
    case None => EitherT.left(Future(s"Power level of $autobot is not known"))

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for
    pl1 <- getPowerLevel(ally1)
    pl2 <- getPowerLevel(ally2)
  yield (pl1 + pl2) > 15

def tacticalReport(ally1: String, ally2: String): String =
  val stack = canSpecialMove(ally1, ally2).value

  Await.result(stack, 1.seconds) match
    case Left(msg) => s"Comms error: $msg"
    case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) => s"$ally1 and $ally2 need a recharge."