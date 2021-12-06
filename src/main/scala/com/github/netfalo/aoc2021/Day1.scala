package com.github.netfalo.aoc2021

object Day1 extends Problem {
  override def solveFirstPart(input: String): String = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(_.toInt)
      .sliding(2)
      .count(pair => pair.head < pair(1))
      .toString
  }

  override def solveSecondPart(input: String): String = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count(pair => pair.head < pair(1))
      .toString
  }
}
