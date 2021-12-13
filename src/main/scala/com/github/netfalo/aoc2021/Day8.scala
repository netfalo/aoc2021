package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day8 extends Problem[Int, Int] {

  case class Digit(d: Int, size: Int) {
    def &(that: Digit): Digit = {
      val value = that.d & d
      var _d = value
      var length = 0
      while (_d != 0) {
        length += _d & 1
        _d >>= 1
      }

      Digit(value, length)
    }
  }

  case class Display(usp: Vector[Digit], digits: Vector[Digit]) {
    def decoder: Map[Digit, Int] = {
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
            case digit if rtable.contains(4) && rtable.contains(8) && (rtable(4) & digit) == rtable(4) =>
              decode(remaningUsp.tail, table.updated(digit, 9), rtable.updated(9, digit))
            case digit if rtable.contains(1) && rtable.contains(9) && digit.size == 6 && (rtable(1) & digit) == rtable(1) =>
              decode(remaningUsp.tail, table.updated(digit, 0), rtable.updated(0, digit))
            case digit if rtable.contains(0) && rtable.contains(9) && digit.size == 6 =>
              decode(remaningUsp.tail, table.updated(digit, 6), rtable.updated(6, digit))
            case digit if rtable.contains(6) && (rtable(6) & digit) == digit && digit.size == 5 =>
              decode(remaningUsp.tail, table.updated(digit, 5), rtable.updated(5, digit))
            case digit if rtable.contains(1) && (rtable(1) & digit) == rtable(1) && digit.size == 5 =>
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

    def content: Int = digits
      .map(decoder)
      .foldLeft(0)((acc, n) => acc * 10 + n)
  }

  def parseDisplay(input: String): Display = {
    val Array(usp, digits) = input.split('|')
    Display(usp.split(' ')
      .filterNot(_.isBlank)
      .map(parseDigit)
      .toVector,
      digits.split(' ')
        .filterNot(_.isBlank)
        .map(parseDigit)
        .toVector)
  }

  private def parseDigit(x: String): Digit = {
    val d = x.map {
      case 'a' => 1 << 0
      case 'b' => 1 << 1
      case 'c' => 1 << 2
      case 'd' => 1 << 3
      case 'e' => 1 << 4
      case 'f' => 1 << 5
      case 'g' => 1 << 6
    }
      .foldLeft(0)((acc, n) => acc | n)

    Digit(d, x.length)
  }

  def parseLines(input: String): Vector[Display] = {
    input
      .split('\n')
      .filter(!_.isBlank)
      .map(parseDisplay)
      .toVector
  }

  override def solveFirstPart(input: String): Int =
    parseLines(input)
      .map(_.digits.count(x => Set(2, 3, 4, 7).contains(x.size)))
      .sum

  override def solveSecondPart(input: String): Int =
    parseLines(input)
      .map(_.content)
      .sum
}
