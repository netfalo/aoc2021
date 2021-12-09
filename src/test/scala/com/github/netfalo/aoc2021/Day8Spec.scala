package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day8.Display
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day8Spec extends AnyFlatSpec {
  private val smallExample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"
  private val largeExample =
    """
      |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
      |""".stripMargin
  private lazy val exercise = Resource("Day8.txt").content

  "Day 8" should "parse display" in {
    val input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"
    val usp = Vector("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab")
      .map(_.toSet)
    val digits = Vector("cdfeb", "fcadb", "cdfeb", "cdbaf").map(_.toSet)
    val expected = Display(usp, digits)

    Day8.parseDisplay(input) shouldEqual expected
  }

  "Day8" should "solve part 1" in {
    Day8.solveFirstPart(largeExample) shouldEqual "26"
    Day8.solveFirstPart(exercise) shouldEqual "264"
  }

  "Day8" should "decode" in {
    Day8.parseDisplay(smallExample).content shouldEqual 5353
  }

  "Day8" should "solve part 2" in {
    Day8.solveSecondPart(largeExample) shouldEqual "61229"
    Day8.solveSecondPart(exercise) shouldEqual "1063760"
  }

}
