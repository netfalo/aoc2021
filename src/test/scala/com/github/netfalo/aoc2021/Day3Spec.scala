package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day3Spec extends AnyFlatSpec {
  private val example =
    """
      |00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |""".stripMargin

  private lazy val exercise = Resource("Day3.txt").content

  "the stuff" should "apple" in {
    Day3.calculateGammaRate(Day3.parseReport(example)) shouldEqual 22
    Day3.solveFirstPart(example) shouldEqual "198"

    Day3.solveFirstPart(exercise) shouldEqual "741950"
  }

  "the second stuff" should "apple" in {
    Day3.getOxygenGeneratorRating(Day3.parseReport(example)) shouldEqual 23
    Day3.getCo2ScrubbingRating(Day3.parseReport(example)) shouldEqual 10

    Day3.solveSecondPart(example) shouldEqual "230"
    Day3.solveSecondPart(exercise) shouldEqual "903810"
  }

}
