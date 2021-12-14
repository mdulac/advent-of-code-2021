package tech.dulac.aoc

import scala.annotation.tailrec

object Day14 extends App {

  val template = input.head

  val transformations = input.collect { case s"$couple -> $result" => (couple, result) }.toMap

  @tailrec
  def step(n: Int)(couples: Map[String, Long]): Map[String, Long] = n match {
    case n if n <= 0 => couples
    case n =>
      val newCouples = couples.foldLeft(Map.empty[String, Long].withDefaultValue(0L)) { case (acc, (couple, i)) =>
        val couple1      = s"${couple.head}${transformations(couple)}"
        val couple2      = s"${transformations(couple)}${couple.last}"
        val couple1Count = acc.getOrElse(couple1, 0L) + i
        val couple2Count = acc.getOrElse(couple2, 0L) + i
        acc.updated(couple1, couple1Count).updated(couple2, couple2Count)
      }
      step(n - 1)(newCouples)
  }

  val tenSteps   = step(10)(template.sliding(2).map((_, 1L)).toMap)
  val fourtyStep = step(40)(template.sliding(2).map((_, 1L)).toMap)

  def occurrences(map: Map[String, Long]) =
    map.groupMap { case (couple, _) => couple.tail } { case (_, value) => value }.view.mapValues(_.sum).toMap

  val part1 = occurrences(tenSteps)
  val part2 = occurrences(fourtyStep)

  println(part1.maxBy(_._2)._2 - part1.minBy(_._2)._2)
  println(part2.maxBy(_._2)._2 - part2.minBy(_._2)._2)

  lazy val example = List(
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  )

  lazy val input = List(
    "KHSNHFKVVSVPSCVHBHNP",
    "",
    "FV -> H",
    "SB -> P",
    "NV -> S",
    "BS -> K",
    "KB -> V",
    "HB -> H",
    "NB -> N",
    "VB -> P",
    "CN -> C",
    "CF -> N",
    "OF -> P",
    "FO -> K",
    "OC -> F",
    "BN -> V",
    "PO -> O",
    "OS -> B",
    "KH -> N",
    "BB -> C",
    "PV -> K",
    "ON -> K",
    "NF -> H",
    "BV -> K",
    "SN -> N",
    "PB -> S",
    "PK -> F",
    "PF -> S",
    "BP -> K",
    "SP -> K",
    "NN -> K",
    "FP -> N",
    "NK -> N",
    "SF -> P",
    "HS -> C",
    "OH -> C",
    "FS -> H",
    "VH -> N",
    "CO -> P",
    "VP -> H",
    "FF -> N",
    "KP -> B",
    "BH -> B",
    "PP -> F",
    "SS -> P",
    "CV -> S",
    "HO -> P",
    "PN -> K",
    "SO -> O",
    "NO -> O",
    "NH -> V",
    "HH -> F",
    "KK -> C",
    "VO -> B",
    "KS -> B",
    "SV -> O",
    "OP -> S",
    "VK -> H",
    "KF -> O",
    "CP -> H",
    "SH -> H",
    "NC -> S",
    "KC -> O",
    "CK -> H",
    "CH -> B",
    "KO -> O",
    "OV -> P",
    "VF -> V",
    "HN -> P",
    "FH -> P",
    "BC -> V",
    "HV -> N",
    "BO -> V",
    "PH -> P",
    "NP -> F",
    "FN -> F",
    "FK -> P",
    "SC -> C",
    "KN -> S",
    "NS -> S",
    "OK -> S",
    "HK -> O",
    "PC -> O",
    "BK -> O",
    "OO -> P",
    "BF -> N",
    "SK -> V",
    "VS -> B",
    "HP -> H",
    "VC -> V",
    "KV -> P",
    "FC -> H",
    "HC -> O",
    "HF -> S",
    "CB -> H",
    "CC -> B",
    "PS -> C",
    "OB -> B",
    "CS -> S",
    "VV -> S",
    "VN -> H",
    "FB -> N"
  )

}
