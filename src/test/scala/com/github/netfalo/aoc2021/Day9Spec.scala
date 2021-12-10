package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day9.HeatMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day9Spec extends AnyFlatSpec {

  private val example =
    """
      |2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |""".stripMargin

  private lazy val exercise = Resource("Day9.txt").content

  "Day9" should "parse input" in {
    val expected = HeatMap(Vector(
      Vector(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
      Vector(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
      Vector(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
      Vector(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
      Vector(9, 8, 9, 9, 9, 6, 5, 6, 7, 8),
    )
    )
    Day9.parseInput(example) shouldBe expected
  }

  "HeatMap" should "give neighbours" in {
    Day9.parseInput(example).neighbours(0, 0) shouldBe Vector((1, 0), (0, 1))
    Day9.parseInput(example).neighbours(4, 0) shouldBe Vector((3, 0), (4, 1))
    Day9.parseInput(example).neighbours(0, 9) shouldBe Vector((0,8), (1,9))
    Day9.parseInput(example).neighbours(4, 9) shouldBe Vector((3,9), (4,8))
    Day9.parseInput(example).neighbours(2, 5) shouldBe Vector((1,5), (2,4), (3,5), (2,6))
  }


  "HeatMap" should "give local minimums" in {
    Day9.parseInput(example).localMinimums shouldBe Vector((0, 1), (0, 9), (2, 2), (4, 6))
  }

  "HeatMap" should "give basin" in {
    Day9.parseInput(example).basin(0, 1) shouldBe Vector((1,0), (0,0), (0,1))
    Day9.parseInput(example).basin(0, 9) shouldBe Vector((0,5), (1,6), (0,6), (0,7), (1,8), (0,8), (2,9), (1,9), (0,9))
    Day9.parseInput(example).basin(2, 2) shouldBe Vector((1,2), (2,1), (3,0), (4,1), (3,1), (3,4), (3,3), (3,2), (1,4), (1,3), (2,5), (2,4), (2,3), (2,2))
    Day9.parseInput(example).basin(4, 6) shouldBe Vector((2,7), (3,8), (3,7), (3,6), (4,5), (4,9), (4,8), (4,7), (4,6))
  }

  "HeatMap" should "calculate total risk level" in {
    Day9.parseInput(example).totalRiskLevel shouldBe 15
    Day9.parseInput(exercise).totalRiskLevel shouldBe 486
  }

  "HeatMap" should "calculate basiness level" in {
    Day9.parseInput(example).sumBasinAreas shouldBe 1134
    Day9.parseInput(exercise).sumBasinAreas shouldBe 1059300
  }

}
