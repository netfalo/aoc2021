package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  private lazy val input = Resource("Day1.txt").content

  "The Day1 object first problem" should "be 1832" in {
      Day1.solveFirstPart(input) shouldEqual 1832
  }

  "The Day1 object second problem" should "be 1858" in {
    Day1.solveSecondPart(input) shouldEqual 1858
  }
}
