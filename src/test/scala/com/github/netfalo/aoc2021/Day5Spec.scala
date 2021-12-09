package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day5.{Line, Point}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day5Spec extends AnyFlatSpec {
  private val example = """
                          |0,9 -> 5,9
                          |8,0 -> 0,8
                          |9,4 -> 3,4
                          |2,2 -> 2,1
                          |7,0 -> 7,4
                          |6,4 -> 2,0
                          |0,9 -> 2,9
                          |3,4 -> 1,4
                          |0,0 -> 8,8
                          |5,5 -> 8,2
                          |""".stripMargin

  private lazy val exercise = Resource("Day5.txt").content

  "Day5" should "parsePoint" in {
    Day5.parsePoint("8,8") shouldEqual Point(8,8)
    Day5.parsePoint("833,133") shouldEqual Point(833,133)
  }

  "Day5" should "parseLine" in {
    Day5.parseLine("3,4 -> 1,4") shouldEqual Line(Point(3,4), Point(1,4))
    Day5.parseLine("823,952 -> 177,306") shouldEqual Line(Point(823,952), Point(177,306))
  }

  "Day5" should "solve first" in {
    Day5.solveFirstPart(example) shouldEqual "5"
    Day5.solveFirstPart(exercise) shouldEqual "7468"
  }

  "Day5" should "get all points" in {
    Day5.parseLine("9,7 -> 7,9").getAllPoints shouldEqual Vector(Point(9,7), Point(8,8), Point(7,9))
    Day5.parseLine("9,7 -> 7,7").getAllPoints shouldEqual Vector(Point(9,7), Point(8,7), Point(7,7))
  }

  "Day5" should "solve second" in {
    Day5.solveSecondPart(example) shouldEqual "12"
    Day5.solveSecondPart(exercise) shouldEqual "22364"
  }

}
