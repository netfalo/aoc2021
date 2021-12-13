package com.github.netfalo.aoc2021

object Day7 extends Problem[Int, Int] {
  def parseInput(input: String): Vector[Int] = input.split(',').map(Integer.parseInt).toVector

  override def solveFirstPart(input: String): Int = {
    val crabs = parseInput(input)
    val median = crabs.sorted.apply(crabs.size / 2)

    crabs
      .map(distance(_, median))
      .sum
  }

  private def distance(crab: Int, x: Int): Int = Math.abs(x - crab)

  override def solveSecondPart(input: String): Int = {
    val crabs = parseInput(input)
    val mean = crabs.sum / crabs.size

    def kicsiGauss(n: Int): Int = n * (n + 1) / 2

    val fuelToFloor =
      crabs
        .map(x => kicsiGauss(distance(mean, x)))
        .sum

    val fuelToCeil = crabs
      .map(x => kicsiGauss(distance(mean + 1, x)))
      .sum

    fuelToCeil min fuelToFloor
  }
}
