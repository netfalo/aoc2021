package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day4.{Board, Free}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day4Spec extends AnyFlatSpec {
  private val example =
    """
      |7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin

  private val exercise = Resource("Day4.txt").content

  "parser" should "parse" in {
    val board =
      """
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        |""".stripMargin
    val expected = Board(
      Vector(
        Vector(Free("22"), Free("13"), Free("17"), Free("11"), Free("0")),
        Vector(Free("8"), Free("2"), Free("23"), Free("4"), Free("24")),
        Vector(Free("21"), Free("9"), Free("14"), Free("16"), Free("7")),
        Vector(Free("6"), Free("10"), Free("3"), Free("18"), Free("5")),
        Vector(Free("1"), Free("12"), Free("20"), Free("15"), Free("19"))
      )
    )

    Day4.parseBoard(board.split('\n')) shouldEqual expected

    Day4.parseGame(example).boards.size shouldEqual 3

    val expectedDrawnNumbers = Vector(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1).map(_.toString)
    Day4.parseGame(example).drawNumbers shouldEqual expectedDrawnNumbers

    Day4.parseGame(example).playToWin() shouldEqual 4512
    Day4.parseGame(exercise).playToWin() shouldEqual 35670
  }

  "The second stuff" should "win" in {
    Day4.parseGame(example).playToLoose() shouldEqual 1924
    Day4.parseGame(exercise).playToLoose() shouldEqual 22704
  }

}
