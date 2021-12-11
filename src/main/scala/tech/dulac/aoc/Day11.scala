package tech.dulac.aoc

import scala.annotation.tailrec

object Day11 extends App {

  private type Grid    = List[Int]
  private type Indices = List[Int]
  private type Flashes = Int
  private type Step    = Int

  private val validIndex = (i: Int) => i >= 0 && i < values.length

  def getNeighboursFor(index: Int) = {
    val top         = index - input.head.length
    val topRight    = index - input.head.length + 1
    val topLeft     = index - input.head.length - 1
    val right       = index + 1
    val bottomRight = index + input.head.length + 1
    val bottom      = index + input.head.length
    val bottomLeft  = index + input.head.length - 1
    val left        = index - 1

    if ((index + 1) % input.head.length == 0) List(left, top, bottom, topLeft, bottomLeft).filter(validIndex)
    else if (index % input.head.length == 0) List(right, top, bottom, topRight, bottomRight).filter(validIndex)
    else List(left, right, top, bottom, topRight, topLeft, bottomRight, bottomLeft).filter(validIndex)
  }

  @tailrec
  def increment(grid: Grid)(indices: Indices)(newZeros: Indices): (Grid, Indices) = indices match {
    case i :: is =>
      if (grid(i) == 0) increment(grid)(is)(newZeros)
      else if (grid(i) == 9) increment(grid.updated(i, 0))(is)(i :: newZeros)
      else increment(grid.updated(i, grid(i) + 1))(is)(newZeros)
    case _ => (grid, newZeros)
  }

  def incrementAll(grid: Grid): Grid = grid.map {
    case x if x >= 9 => 0
    case x           => x + 1
  }

  def getZeroIndices(grid: Grid) = grid.zipWithIndex.foldLeft(List.empty[Int]) { case (indices, (value, index)) =>
    if (value == 0) index :: indices
    else indices
  }

  @tailrec
  def processZeroIndices(grid: Grid)(indices: Indices): Grid = indices match {
    case i :: is =>
      val neighbours          = getNeighboursFor(i)
      val (newGrid, newZeros) = increment(grid)(neighbours)(List.empty)
      processZeroIndices(newGrid)((newZeros ++ is).distinct)
    case _ => grid
  }

  def run(grid: Grid): Grid = {
    val g           = incrementAll(grid)
    val zeroIndices = getZeroIndices(g)
    processZeroIndices(g)(zeroIndices)
  }

  @tailrec
  def steps(n: Step)(grid: Grid)(flashes: Flashes): Flashes =
    n match {
      case 0 => flashes
      case n =>
        val newGrid = run(grid)
        steps(n - 1)(newGrid)(flashes + newGrid.count(_ == 0))
    }

  @tailrec
  def stepsUntilAllFlashed(grid: Grid)(step: Step): Step =
    if (grid.forall(_ == 0)) step
    else {
      val newGrid = run(grid)
      stepsUntilAllFlashed(newGrid)(step + 1)
    }

  private val part1 = steps(100)(values)(0)
  private val part2 = stepsUntilAllFlashed(values)(0)

  println(part1)
  println(part2)

  lazy val values = input.mkString.map(c => Integer.parseInt(c.toString)).toList
  lazy val input = List(
    "3172537688",
    "4566483125",
    "6374512653",
    "8321148885",
    "4342747758",
    "1362188582",
    "7582213132",
    "6887875268",
    "7635112787",
    "7242787273"
  )

}
