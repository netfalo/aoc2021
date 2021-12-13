package com.github.netfalo.aoc2021

object Day6 extends Problem[Long, Long] {

  def parseInput(input: String): Vector[Long] = {
    input.split(',')
      .map(Integer.parseInt)
      .groupBy(identity)
      .map(x => (x._1, x._2.length))
      .foldLeft(Vector.fill(9)(0L))((acc, n) => acc.updated(n._1, n._2))
  }

  override def solveFirstPart(input: String): Long =
    numberOfFishesAfterDays(parseInput(input), 80)

  override def solveSecondPart(input: String): Long =
    numberOfFishesAfterDays(parseInput(input), 256)

  private def numberOfFishesAfterDays(numberOfFishes: Vector[Long], n: Int): Long = {
    LazyList
      .from(0)
      .take(n)
      .foldLeft(numberOfFishes) { case (acc, _) => simulateDay(acc) }
      .sum
  }

  private def simulateDay(acc: Vector[Long]): Vector[Long] = {
    val newBorn = acc.head
    val notPregnant = acc
      .tail
    if (newBorn == 0) {
      notPregnant
        .appended(0)
    } else {
      notPregnant
        .updated(6, notPregnant(6) + newBorn)
        .appended(newBorn)
    }
  }
}
