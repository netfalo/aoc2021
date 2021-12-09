package com.github.netfalo.aoc2021

import scala.language.implicitConversions

object Day7 extends Problem {
  def parseInput(input: String): Vector[Int] = input.split(',').map(Integer.parseInt).toVector

  override def solveFirstPart(input: String): String = {
    val crabs = parseInput(input)
    Range.inclusive(crabs.min, crabs.max)
      .map(crab => crabs.map(distance(crab, _)).sum)
      .min
      .toString
  }

  private def distance(crab: Int, x: Int): Int = Math.abs(x - crab)

  override def solveSecondPart(input: String): String = {
    val crabs = parseInput(input)

    def kicsiGauss(n: Int): Int = n * (n + 1) / 2

    Range.inclusive(crabs.min, crabs.max)
      .map(crab => crabs.map(x => kicsiGauss(distance(crab, x))).sum)
      .min
      .toString
  }
}
