package tech.dulac.aoc

object Day12 extends App {

  type Path = List[String]

  val links = input
    .flatMap {
      case s"start-$to"   => List("start" -> to)
      case s"$from-start" => List("start" -> from)
      case s"end-$to"     => List(to -> "end")
      case s"$from-end"   => List(from -> "end")
      case s"$from-$to"   => List(from -> to, to -> from)
    }
    .groupMap(_._1)(_._2)

  def canVisit(cave: String)(visited: Map[String, Int]) =
    !visited.contains(cave) || visited.filter { case (key, _) => key.toLowerCase == key }.values.forall(_ > 0)

  def paths(from: String)(visited: Map[String, Int]): List[Path] = from match {
    case "end" => List("end") :: Nil
    case from if from.toUpperCase == from || canVisit(from)(visited) =>
      val newVisited = visited.updated(from, visited(from) - 1)
      links(from).flatMap(l => paths(l)(newVisited).map(from :: _))
    case _ => List.empty
  }

  val part1 = paths("start")(Map.empty.withDefaultValue(1))
  val part2 = paths("start")(Map.empty.withDefaultValue(2))

  println(part1.length)
  println(part2.length)

  lazy val example = List(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  )

  lazy val input = List(
    "pn-TY",
    "rp-ka",
    "az-aw",
    "al-IV",
    "pn-co",
    "end-rp",
    "aw-TY",
    "rp-pn",
    "al-rp",
    "end-al",
    "IV-co",
    "end-TM",
    "co-TY",
    "TY-ka",
    "aw-pn",
    "aw-IV",
    "pn-IV",
    "IV-ka",
    "TM-rp",
    "aw-PD",
    "start-IV",
    "start-co",
    "start-pn"
  )

}
