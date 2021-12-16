package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
  private val example =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  private val parsedExample = (
    Map(
      "NN" -> 1L,
      "NC" -> 1L,
      "CB" -> 1L),
    Map(
      'N' -> 2L,
      'C' -> 1L,
      'B' -> 1L
    ),
    Map(
      "CH" -> "B",
      "HH" -> "N",
      "CB" -> "H",
      "NH" -> "C",
      "HB" -> "C",
      "HC" -> "B",
      "HN" -> "C",
      "NN" -> "C",
      "BH" -> "H",
      "NC" -> "B",
      "NB" -> "B",
      "BN" -> "B",
      "BB" -> "N",
      "BC" -> "B",
      "CC" -> "N",
      "CN" -> "C"
    ))

  private lazy val exercise = Resource("Day14.txt").content

  "Day14" should "parse" in {
    Day14.parseInput(example) shouldEqual parsedExample
  }

  it should "do one step" in {
    val polymer = Map(
      "NN" -> 1L,
      "NC" -> 1L,
      "CB" -> 1L
    )
    val counts = Map(
      'N' -> 2L,
      'C' -> 1L,
      'B' -> 1L
    )
    val expectedPairs = "NCNBCHB"
      .sliding(2)
      .toList
      .map((_, 1L))
      .groupMapReduce(_._1)(_._2)(_ + _)
    val expectedCounts = "NCNBCHB"
      .toList
      .map((_, 1L))
      .groupMapReduce(_._1)(_._2)(_ + _)

    val expected = (expectedPairs, expectedCounts)
    Day14.doOneInsertion(polymer, parsedExample._3, counts) shouldEqual expected
  }

  it should "do 4 step" in {
    val (polymer, counts, rules) = parsedExample

    val expected = "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
      .toList
      .map((_, 1L))
      .groupMapReduce(_._1)(_._2)(_ + _)

    Day14.doNInsertion(polymer, rules, counts, 4) shouldEqual expected
  }

  it should "solve first part" in {
    Day14.solveFirstPart(example) shouldEqual 1588
    Day14.solveFirstPart(exercise) shouldEqual 2967
  }

  it should "solve second part" in {
    Day14.solveSecondPart(example) shouldEqual 2188189693529L
    Day14.solveSecondPart(exercise) shouldEqual 3692219987038L
  }
}
