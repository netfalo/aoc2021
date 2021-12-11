package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day11.{Grid, Point, parseGrid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  private val smallExample =
    """
      |11111
      |19991
      |19191
      |19991
      |11111
      |""".stripMargin

  private val largeExample =
    """
      |5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526
      |""".stripMargin

  private val smallGrid = Grid(Map(
    Point(0, 0) -> 1, Point(1, 0) -> 1, Point(2, 0) -> 1, Point(3, 0) -> 1, Point(4, 0) -> 1,
    Point(0, 1) -> 1, Point(1, 1) -> 9, Point(2, 1) -> 9, Point(3, 1) -> 9, Point(4, 1) -> 1,
    Point(0, 2) -> 1, Point(1, 2) -> 9, Point(2, 2) -> 1, Point(3, 2) -> 9, Point(4, 2) -> 1,
    Point(0, 3) -> 1, Point(1, 3) -> 9, Point(2, 3) -> 9, Point(3, 3) -> 9, Point(4, 3) -> 1,
    Point(0, 4) -> 1, Point(1, 4) -> 1, Point(2, 4) -> 1, Point(3, 4) -> 1, Point(4, 4) -> 1
  ), Map(
    Point(3, 3) -> List(Point(2, 2), Point(2, 3), Point(2, 4), Point(3, 2), Point(3, 3), Point(3, 4), Point(4, 2), Point(4, 3), Point(4, 4)),
    Point(3, 1) -> List(Point(2, 0), Point(2, 1), Point(2, 2), Point(3, 0), Point(3, 1), Point(3, 2), Point(4, 0), Point(4, 1), Point(4, 2)),
    Point(2, 0) -> List(Point(1, 0), Point(1, 1), Point(2, 0), Point(2, 1), Point(3, 0), Point(3, 1)),
    Point(2, 2) -> List(Point(1, 1), Point(1, 2), Point(1, 3), Point(2, 1), Point(2, 2), Point(2, 3), Point(3, 1), Point(3, 2), Point(3, 3)),
    Point(4, 4) -> List(Point(3, 3), Point(3, 4), Point(4, 3), Point(4, 4)),
    Point(1, 1) -> List(Point(0, 0), Point(0, 1), Point(0, 2), Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 0), Point(2, 1), Point(2, 2)),
    Point(4, 0) -> List(Point(3, 0), Point(3, 1), Point(4, 0), Point(4, 1)),
    Point(0, 0) -> List(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1)),
    Point(4, 3) -> List(Point(3, 2), Point(3, 3), Point(3, 4), Point(4, 2), Point(4, 3), Point(4, 4)),
    Point(2, 3) -> List(Point(1, 2), Point(1, 3), Point(1, 4), Point(2, 2), Point(2, 3), Point(2, 4), Point(3, 2), Point(3, 3), Point(3, 4)),
    Point(0, 1) -> List(Point(0, 0), Point(0, 1), Point(0, 2), Point(1, 0), Point(1, 1), Point(1, 2)),
    Point(1, 2) -> List(Point(0, 1), Point(0, 2), Point(0, 3), Point(1, 1), Point(1, 2), Point(1, 3), Point(2, 1), Point(2, 2), Point(2, 3)),
    Point(0, 2) -> List(Point(0, 1), Point(0, 2), Point(0, 3), Point(1, 1), Point(1, 2), Point(1, 3)),
    Point(1, 3) -> List(Point(0, 2), Point(0, 3), Point(0, 4), Point(1, 2), Point(1, 3), Point(1, 4), Point(2, 2), Point(2, 3), Point(2, 4)),
    Point(0, 4) -> List(Point(0, 3), Point(0, 4), Point(1, 3), Point(1, 4)),
    Point(4, 1) -> List(Point(3, 0), Point(3, 1), Point(3, 2), Point(4, 0), Point(4, 1), Point(4, 2)),
    Point(1, 0) -> List(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1), Point(2, 0), Point(2, 1)),
    Point(2, 4) -> List(Point(1, 3), Point(1, 4), Point(2, 3), Point(2, 4), Point(3, 3), Point(3, 4)),
    Point(3, 2) -> List(Point(2, 1), Point(2, 2), Point(2, 3), Point(3, 1), Point(3, 2), Point(3, 3), Point(4, 1), Point(4, 2), Point(4, 3)),
    Point(4, 2) -> List(Point(3, 1), Point(3, 2), Point(3, 3), Point(4, 1), Point(4, 2), Point(4, 3)),
    Point(0, 3) -> List(Point(0, 2), Point(0, 3), Point(0, 4), Point(1, 2), Point(1, 3), Point(1, 4)),
    Point(3, 0) -> List(Point(2, 0), Point(2, 1), Point(3, 0), Point(3, 1), Point(4, 0), Point(4, 1)),
    Point(1, 4) -> List(Point(0, 3), Point(0, 4), Point(1, 3), Point(1, 4), Point(2, 3), Point(2, 4)),
    Point(2, 1) -> List(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 0), Point(2, 1), Point(2, 2), Point(3, 0), Point(3, 1), Point(3, 2)),
    Point(3, 4) -> List(Point(2, 3), Point(2, 4), Point(3, 3), Point(3, 4), Point(4, 3), Point(4, 4))),
    5, 5)

  private lazy val exercise = Resource("Day11.txt").content

  "Day11" should "parse grid" in {
    Day11.parseGrid(smallExample) shouldEqual smallGrid
  }

  "Grid" should "print" in {
    val expected =
      """11111
        |19991
        |19191
        |19991
        |11111""".stripMargin

    smallGrid.toString shouldEqual expected
  }

  it should "run 1 step" in {
    val expected = Map(
      Point(0, 0) -> 3, Point(1, 0) -> 4, Point(2, 0) -> 5, Point(3, 0) -> 4, Point(4, 0) -> 3,
      Point(0, 1) -> 4, Point(1, 1) -> 0, Point(2, 1) -> 0, Point(3, 1) -> 0, Point(4, 1) -> 4,
      Point(0, 2) -> 5, Point(1, 2) -> 0, Point(2, 2) -> 0, Point(3, 2) -> 0, Point(4, 2) -> 5,
      Point(0, 3) -> 4, Point(1, 3) -> 0, Point(2, 3) -> 0, Point(3, 3) -> 0, Point(4, 3) -> 4,
      Point(0, 4) -> 3, Point(1, 4) -> 4, Point(2, 4) -> 5, Point(3, 4) -> 4, Point(4, 4) -> 3
    )

    val actual = smallGrid.stepN(1)
    actual._1 shouldEqual expected
    actual._2 shouldEqual 9

  }

  it should "run 2 step" in {
    val expected = Map(
      Point(0, 0) -> 4, Point(1, 0) -> 5, Point(2, 0) -> 6, Point(3, 0) -> 5, Point(4, 0) -> 4,
      Point(0, 1) -> 5, Point(1, 1) -> 1, Point(2, 1) -> 1, Point(3, 1) -> 1, Point(4, 1) -> 5,
      Point(0, 2) -> 6, Point(1, 2) -> 1, Point(2, 2) -> 1, Point(3, 2) -> 1, Point(4, 2) -> 6,
      Point(0, 3) -> 5, Point(1, 3) -> 1, Point(2, 3) -> 1, Point(3, 3) -> 1, Point(4, 3) -> 5,
      Point(0, 4) -> 4, Point(1, 4) -> 5, Point(2, 4) -> 6, Point(3, 4) -> 5, Point(4, 4) -> 4
    )

    val actual = smallGrid.stepN(2)
    actual._1 shouldEqual expected
    actual._2 shouldEqual 9
  }

  it should "run 1 step on large" in {
    val expected = parseGrid(
      """6594254334
        |3856965822
        |6375667284
        |7252447257
        |7468496589
        |5278635756
        |3287952832
        |7993992245
        |5957959665
        |6394862637""".stripMargin)

    parseGrid(largeExample).stepN(1) shouldEqual(expected.octopuses, 0)
  }

  it should "run 2 steps" in {
    val expected = parseGrid(
      """8807476555
        |5089087054
        |8597889608
        |8485769600
        |8700908800
        |6600088989
        |6800005943
        |0000007456
        |9000000876
        |8700006848""".stripMargin)

    parseGrid(largeExample).stepN(2) shouldEqual(expected.octopuses, 35)
  }

  it should "run 10 steps" in {
    parseGrid(largeExample).stepN(10)._2 shouldEqual 204
  }

  it should "run 100 steps" in {
    parseGrid(largeExample).stepN(100)._2 shouldEqual 1656
    parseGrid(exercise).stepN(100)._2 shouldEqual 1627
  }

  it should "find the first synchronized flash" in {
    parseGrid(largeExample).firstSynchronizedFlash() shouldEqual 195
    parseGrid(exercise).firstSynchronizedFlash() shouldEqual 329
  }
}
