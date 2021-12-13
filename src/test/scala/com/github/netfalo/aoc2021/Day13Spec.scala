package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {
  private val example =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  private lazy val exercise = Resource("Day13.txt").content

  "Day13" should "parse " in {
    val expected = (Set(Point(6, 10), Point(0, 14), Point(9, 10), Point(0, 3), Point(10, 4), Point(4, 11), Point(6, 0),
      Point(6, 12), Point(4, 1), Point(0, 13), Point(10, 12), Point(3, 4), Point(3, 0), Point(8, 4), Point(1, 10),
      Point(2, 14), Point(8, 10), Point(9, 0)),
      List(Point(0, 7), Point(5, 0)))

    Day13.parseInput(example) shouldEqual expected
  }

  it should "fold one" in {
    Day13.solveFirstPart(example) shouldEqual 17
    Day13.solveFirstPart(exercise) shouldEqual 747
  }

  it should "decode char" in {
    val z = """####
              |...#
              |..#.
              |.#..
              |#...
              |####""".stripMargin

    val Z = z
      .split('\n')
      .zipWithIndex
      .flatMap {case (line, y) => line.zipWithIndex.collect { case ('#', x) => Point(x, y)} }
      .toSet

    Day13.decode(Z) shouldEqual 'Z'
  }

  it should "" in {
    val (points, instructions) = Day13.parseInput(example)
    val expected = Set(
      Point(0,0),Point(1,0),Point(2,0),Point(3,0),Point(4,0),
      Point(0,1),Point(5,1),
      Point(0,2),Point(5,2),
      Point(0,3),Point(5,3),
      Point(0,4),Point(1,4),Point(2,4),Point(3,4),Point(4,4),
    )
    Day13.doEveryFold(points, instructions) shouldEqual expected
  }

  it should "decode licence key" in {
    Day13.solveSecondPart(exercise) shouldEqual "ARHZPCUH"
  }
}
