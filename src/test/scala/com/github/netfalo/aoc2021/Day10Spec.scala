package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day10Spec extends AnyFlatSpec {
  private val example =
    """
      |[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |""".stripMargin

  private val exerciser = Resource("Day10.txt").content

  "Day10" should "parse lines" in {
    Day10.parseLines(example).head shouldEqual "[({(<(())[]>[[{[]{<()<>>"
  }

  it should "tell if line is corrupted" in {
    Day10.isCorrupted("(]") shouldEqual Right(']')
    Day10.isCorrupted("()") shouldEqual Left(Vector())
    Day10.isCorrupted("<([]){()}[{}])") shouldEqual Right(')')

    Day10.isCorrupted("{([(<{}[<>[]}>{[]{[(<()>") shouldEqual Right('}')
    Day10.isCorrupted("[[<[([]))<([[{}[[()]]]") shouldEqual Right(')')
    Day10.isCorrupted("[{[{({}]{}}([{[{{{}}([]") shouldEqual Right(']')
    Day10.isCorrupted("[<(<(<(<{}))><([]([]()") shouldEqual Right(')')
    Day10.isCorrupted("<{([([[(<>()){}]>(<<{{") shouldEqual Right('>')

  }

  it should "solve first part" in {
    Day10.solveFirstPart(example) shouldEqual 26397
    Day10.solveFirstPart(exerciser) shouldEqual 362271
  }


  it should "solve second part" in {
    Day10.solveSecondPart(example) shouldEqual 288957
    Day10.solveSecondPart(exerciser) shouldEqual 1698395182
  }}
