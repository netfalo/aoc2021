package com.github.netfalo.aoc2021

object Day5 extends Problem[Int, Int] {

  sealed trait Orientation

  case object Horizontal extends Orientation

  case object Vertical extends Orientation

  case object Diagonal extends Orientation

  case class Point(x: Int, y: Int)

  case class Line(from: Point, to: Point) {
    val orientation: Orientation = if (from.x == to.x) Vertical else if (from.y == to.y) Horizontal else Diagonal

    def getAllPoints: Vector[Point] = {
      val xs = if (orientation == Vertical)
        Vector.fill(Math.abs(from.y - to.y) + 1)(from.x)
      else if (from.x < to.x)
        Range.inclusive(from.x, to.x)
      else
        Range.inclusive(to.x, from.x).reverse

      val ys = if (orientation == Horizontal)
        Vector.fill(Math.abs(from.x - to.x) + 1)(from.y)
      else if (from.y < to.y)
        Range.inclusive(from.y, to.y)
      else
        Range.inclusive(to.y, from.y).reverse

      xs.zip(ys)
        .map(pair => Point(pair._1, pair._2))
        .toVector
    }

    def intersection(that: Line): Vector[Point] = {
      if (orientation == Horizontal && that.orientation == orientation) {
        Vector()
      } else if (orientation == Vertical && that.orientation == orientation) {
        Vector()
      } else {
        throw new NotImplementedError
      }
    }

    //Is a between x and y
    private def between(x: Int, y: Int, a: Int): Boolean = {
      (a > x && y > a) || (a > y && x > a)
    }
  }

  private val pointPattern = "(\\d+),(\\d+)".r

  def parsePoint(input: String): Point = input match {
    case pointPattern(x, y) => Point(Integer.parseInt(x), Integer.parseInt(y))
    case _ => throw new IllegalArgumentException
  }

  private val linePattern = "([0-9,]+) -> ([0-9,]+)".r

  def parseLine(input: String): Line = input match {
    case linePattern(from, to) => Line(parsePoint(from), parsePoint(to))
    case x => throw new IllegalArgumentException(s"Cannot parse '$x'")
  }

  def parseLines(input: String): Vector[Line] =
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(parseLine)
      .toVector

  override def solveFirstPart(input: String): Int =
    parseLines(input)
      .filter(line => line.orientation == Horizontal || line.orientation == Vertical)
      .flatMap(_.getAllPoints)
      .groupBy(point => point)
      .count(_._2.size > 1)

  override def solveSecondPart(input: String): Int =
    parseLines(input)
      .flatMap(_.getAllPoints)
      .groupBy(point => point)
      .count(_._2.size > 1)

}
