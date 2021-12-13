package com.github.netfalo.aoc2021

trait Problem[T, R] {
  def solveFirstPart(input: String): T
  def solveSecondPart(input: String): R
}
