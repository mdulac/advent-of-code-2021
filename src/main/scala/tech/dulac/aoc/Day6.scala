package tech.dulac.aoc

import scala.annotation.tailrec

object Day6 extends App {

  val count = { case (i: Int, v: List[Int]) => (i, v.size) }

  def nextState = (s: Map[Int, Long]) => {
    val zeros  = s.getOrElse(0, 0L)
    val sevens = s.getOrElse(7, 0L)
    val count =
      (8 -> zeros) +: (6 -> (sevens + zeros)) +: List(0, 1, 2, 3, 4, 5, 7).map(i => i -> s.getOrElse(i + 1, 0L))
    Map.from(count)
  }

  @tailrec
  def simulateForDays(remainingDays: Int)(state: Map[Int, Long]): Map[Int, Long] = remainingDays match {
    case remains if remains < 1 => state
    case remains                => simulateForDays(remains - 1)(nextState(state))
  }

  val part1 = simulateForDays(80)(input.groupBy(identity).map(count)).values.sum
  val part2 = simulateForDays(256)(input.groupBy(identity).map(count)).values.sum

  println(part1)
  println(part2)

  lazy val input = List(
    3, 5, 1, 5, 3, 2, 1, 3, 4, 2, 5, 1, 3, 3, 2, 5, 1, 3, 1, 5, 5, 1, 1, 1, 2, 4, 1, 4, 5, 2, 1, 2, 4, 3, 1, 2, 3, 4, 3,
    4, 4, 5, 1, 1, 1, 1, 5, 5, 3, 4, 4, 4, 5, 3, 4, 1, 4, 3, 3, 2, 1, 1, 3, 3, 3, 2, 1, 3, 5, 2, 3, 4, 2, 5, 4, 5, 4, 4,
    2, 2, 3, 3, 3, 3, 5, 4, 2, 3, 1, 2, 1, 1, 2, 2, 5, 1, 1, 4, 1, 5, 3, 2, 1, 4, 1, 5, 1, 4, 5, 2, 1, 1, 1, 4, 5, 4, 2,
    4, 5, 4, 2, 4, 4, 1, 1, 2, 2, 1, 1, 2, 3, 3, 2, 5, 2, 1, 1, 2, 1, 1, 1, 3, 2, 3, 1, 5, 4, 5, 3, 3, 2, 1, 1, 1, 3, 5,
    1, 1, 4, 4, 5, 4, 3, 3, 3, 3, 2, 4, 5, 2, 1, 1, 1, 4, 2, 4, 2, 2, 5, 5, 5, 4, 1, 1, 5, 1, 5, 2, 1, 3, 3, 2, 5, 2, 1,
    2, 4, 3, 3, 1, 5, 4, 1, 1, 1, 4, 2, 5, 5, 4, 4, 3, 4, 3, 1, 5, 5, 2, 5, 4, 2, 3, 4, 1, 1, 4, 4, 3, 4, 1, 3, 4, 1, 1,
    4, 3, 2, 2, 5, 3, 1, 4, 4, 4, 1, 3, 4, 3, 1, 5, 3, 3, 5, 5, 4, 4, 1, 2, 4, 2, 2, 3, 1, 1, 4, 5, 3, 1, 1, 1, 1, 3, 5,
    4, 1, 1, 2, 1, 1, 2, 1, 2, 3, 1, 1, 3, 2, 2, 5, 5, 1, 5, 5, 1, 4, 4, 3, 5, 4, 4
  )

}
