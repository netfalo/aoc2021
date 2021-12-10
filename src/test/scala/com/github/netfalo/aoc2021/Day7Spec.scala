package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day7Spec extends AnyFlatSpec {
  private val example = "16,1,2,0,4,2,7,1,2,14"
  private lazy val exercise = Resource("Day7.txt").content
  "First part" should "solve" in {
    Day7.solveFirstPart(example) shouldEqual 37
    Day7.solveFirstPart(exercise) shouldEqual 335271
  }

  "Second part" should "solve" in {
    Day7.solveSecondPart(example) shouldEqual 168
    Day7.solveSecondPart(exercise) shouldEqual 95851339
  }

}
