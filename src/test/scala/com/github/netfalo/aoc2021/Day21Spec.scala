package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day21.{parseInput, solveFirstPart, solveSecondPart}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Spec extends AnyFlatSpec with Matchers {
  private val example =
    """
      |Player 1 starting position: 4
      |Player 2 starting position: 8
      |""".stripMargin

  private lazy val exercise = Resource("Day21.txt").content

  "Day21" should "parse" in {
    parseInput(example) shouldEqual (4, 8)
  }

  it should "solve first part" in {
    solveFirstPart(example) shouldEqual 739785
    solveFirstPart(exercise) shouldEqual 918081
  }

  it should "solve second part" in {           177791127L
    solveSecondPart(example) shouldEqual 444356092776315L
    //solveSecondPart(exercise) shouldEqual 0L
  }

}
