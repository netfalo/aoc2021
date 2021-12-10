package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day6Spec extends AnyFlatSpec {
  private val example = "3,4,3,1,2"
  private lazy val exercise = Resource("Day6.txt").content

  "Day6" should "parse fishes" in {
    Day6.parseInput(example) shouldEqual Vector(0, 1, 1, 2, 1, 0, 0, 0, 0)
  }

  "Day6" should "calculate number of fishes at day 80" in {
    Day6.solveFirstPart(example) shouldEqual 5934
    Day6.solveFirstPart(exercise) shouldEqual 396210
  }

  "Day6" should "calculate number of fishes at day 256" in {
    Day6.solveSecondPart(example) shouldEqual 26984457539L
    Day6.solveSecondPart(exercise) shouldEqual 1770823541496L
  }

}
