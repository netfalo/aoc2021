package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day8 extends Problem {

  type Digit = Set[Char]

  case class Display(usp: Vector[Digit], digits: Vector[Digit]) {
    lazy val decoder: Map[Digit, Int] = {
      @tailrec
      def decode(remaningUsp: Vector[Digit], table: Map[Digit, Int], rtable: Map[Int, Digit]): Map[Digit, Int] = {
        if (remaningUsp.isEmpty)
          table
        else {
          remaningUsp.head match {
            case digit if digit.size == 2 =>
              decode(remaningUsp.tail, table.updated(digit, 1), rtable.updated(1, digit))
            case digit if digit.size == 3 =>
              decode(remaningUsp.tail, table.updated(digit, 7), rtable.updated(7, digit))
            case digit if digit.size == 4 =>
              decode(remaningUsp.tail, table.updated(digit, 4), rtable.updated(4, digit))
            case digit if digit.size == 7 =>
              decode(remaningUsp.tail, table.updated(digit, 8), rtable.updated(8, digit))
            case digit if rtable.contains(4) && rtable.contains(8) && rtable(4).intersect(digit) == rtable(4) =>
              decode(remaningUsp.tail, table.updated(digit, 9), rtable.updated(9, digit))
            case digit if rtable.contains(1) && rtable.contains(9) && digit.size == 6 && rtable(1).intersect(digit) == rtable(1) =>
              decode(remaningUsp.tail, table.updated(digit, 0), rtable.updated(0, digit))
            case digit if rtable.contains(0) && rtable.contains(9) && digit.size == 6 =>
              decode(remaningUsp.tail, table.updated(digit, 6), rtable.updated(6, digit))
            case digit if rtable.contains(6) && rtable(6).intersect(digit) == digit && digit.size == 5 =>
              decode(remaningUsp.tail, table.updated(digit, 5), rtable.updated(5, digit))
            case digit if rtable.contains(1) && rtable(1).intersect(digit) == rtable(1) && digit.size == 5 =>
              decode(remaningUsp.tail, table.updated(digit, 3), rtable.updated(3, digit))
            case digit if rtable.size == 9 =>
              decode(remaningUsp.tail, table.updated(digit, 2), rtable.updated(2, digit))
            case digit =>
              decode(remaningUsp.tail.appended(digit), table, rtable)
          }
        }
      }

      decode(usp, Map(), Map())
    }

    lazy val content: Int = digits
      .map(decoder)
      .foldLeft(0)((acc, n) => acc * 10 + n)
  }

  def parseDisplay(input: String): Display = {
    val parts = input.split('|')
    Display(parts(0).split(' ').filterNot(_.isBlank).map(_.toSet).toVector,
      parts(1).split(' ').filterNot(_.isBlank).map(_.toSet).toVector)
  }

  def parseLines(input: String): Vector[Display] = {
    input
      .split('\n')
      .filter(!_.isBlank)
      .map(parseDisplay)
      .toVector
  }

  override def solveFirstPart(input: String): String =
    parseLines(input)
      .map(_.digits.count(x => Set(2, 3, 4, 7).contains(x.size)))
      .sum
      .toString


  override def solveSecondPart(input: String): String =
    parseLines(input)
      .map(_.content)
      .sum
      .toString
}
