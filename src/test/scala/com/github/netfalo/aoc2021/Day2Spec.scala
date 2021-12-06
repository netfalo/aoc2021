package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day2.{Direction, Down, Forward}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec  extends AnyFlatSpec with Matchers {
  val example: String =
    """
      |forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2
      |""".stripMargin

  val commands: Array[Direction] = Array(
    Forward(5),
    Down(5),
    Forward(8),
    Down(-3),
    Down(8),
    Forward(2)
  )

  val exercise = Resource("Day2.txt").content

  "The Day2 object parse" should "be apple" in {
    Day2.parse("forward 2") shouldEqual Forward(2)
    Day2.parse("down 2") shouldEqual Down(2)
    Day2.parse("up 2") shouldEqual Down(-2)

    assertThrows[RuntimeException] {
      Day2.parse("Up 2") shouldEqual Down(2)
    }

    Day2.solveFirstPart(example) shouldEqual "150"
    Day2.solveFirstPart(exercise) shouldEqual "2272262"
  }

  "The Day2 object second problem" should "be second" in {
    Day2.solveSecondPart(example) shouldEqual "900"
    Day2.solveSecondPart(exercise) shouldEqual "2134882034"
  }
}
