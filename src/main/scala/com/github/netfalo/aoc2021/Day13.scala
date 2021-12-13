package com.github.netfalo.aoc2021

object Day13 extends Problem[Long, String] {
  private val pointPattern = "(\\d+),(\\d+)".r
  private val instructionPattern = "fold along ([xy])=(\\d+)".r

  //.##.
  //#..#
  //#..#
  //####
  //#..#
  //#..#
  val A = Set(
    Point(1, 0), Point(2, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2), Point(3, 2),
    Point(0, 3), Point(1, 3), Point(2, 3), Point(3, 3),
    Point(0, 4), Point(3, 4),
    Point(0, 5), Point(3, 5))

  val B = Set(Point(0, 0))

  //.##.
  //#..#
  //#...
  //#...
  //#..#
  //.##.
  val C = Set(
    Point(1, 0), Point(2, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2),
    Point(0, 3),
    Point(0, 4), Point(3, 4),
    Point(1, 5), Point(2, 5))

  //#..#
  //#..#
  //####
  //#..#
  //#..#
  //#..#
  val H = Set(
    Point(0, 0), Point(3, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2), Point(1, 2), Point(2, 2), Point(3, 2),
    Point(0, 3), Point(3, 3),
    Point(0, 4), Point(3, 4),
    Point(0, 5), Point(3, 5))

  //###.
  //#..#
  //#..#
  //###.
  //#...
  //#...
  val P = Set(
    Point(0, 0), Point(1, 0), Point(2, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2), Point(3, 2),
    Point(0, 3), Point(1, 3), Point(2, 3),
    Point(0, 4),
    Point(0, 5))

  //###.
  //#..#
  //#..#
  //###.
  //#.#.
  //#..#
  val R = Set(
    Point(0, 0), Point(1, 0), Point(2, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2), Point(3, 2),
    Point(0, 3), Point(1, 3), Point(2, 3),
    Point(0, 4), Point(2, 4),
    Point(0, 5), Point(3, 5))

  //#..#
  //#..#
  //#..#
  //#..#
  //#..#
  //.##.
  val U = Set(
    Point(0, 0), Point(3, 0),
    Point(0, 1), Point(3, 1),
    Point(0, 2), Point(3, 2),
    Point(0, 3), Point(3, 3),
    Point(0, 4), Point(3, 4),
    Point(1, 5), Point(2, 5)
  )

  //####
  //...#
  //..#.
  //.#..
  //#...
  //####
  val Z = Set(
    Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0),
    Point(3, 1),
    Point(2, 2),
    Point(1, 3),
    Point(0, 4),
    Point(0, 5), Point(1, 5), Point(2, 5), Point(3, 5)
  )


  def parseInput(input: String): (Set[Point], List[Point]) =
    input
      .split('\n')
      .filterNot(_.isBlank)
      .foldLeft((Set[Point](), List[Point]())) {
        case ((points, instructions), pointPattern(x, y)) => (points + Point(Integer.parseInt(x), Integer.parseInt(y)), instructions)
        case ((points, instructions), instructionPattern(axis, p)) => (points, instructions.appended(axis match {
          case "x" => Point(Integer.parseInt(p), 0)
          case "y" => Point(0, Integer.parseInt(p))
        }))
      }

  def doOneFold(points: Set[Point], axis: Point): Int =
    points
      .map(_.mirror(axis))
      .size

  def doEveryFold(points: Set[Point], instructions: List[Point]): Set[Point] = {
    instructions
      .foldLeft(points) {
        case (points, axis) => points.map(point => point.mirror(axis))
      }
  }

  override def solveFirstPart(input: String): Long = {
    val (points, firstFold :: _) = parseInput(input)

    doOneFold(points, firstFold)
  }

  def printChar(char: Set[Point]): Unit = {
    val xs = Range(0, char.map(_.x).max + 1)
    val ys = Range(0, char.map(_.y).max + 1)

    ys
      .foreach(y => {
        xs.foreach(x =>
          if (char.contains(Point(x, y))) print('#')
          else print('.')
        )
        println
      })
    println
  }

  def decode(char: Set[Point]): Char = {
    if (char == A) 'A'
    else if (char == B) 'B'
    else if (char == C) 'C'
    else if (char == H) 'H'
    else if (char == U) 'U'
    else if (char == Z) 'Z'
    else if (char == P) 'P'
    else if (char == R) 'R'
    else ' '
  }

  override def solveSecondPart(input: String): String = {
    val (points, instructions) = parseInput(input)

    val result = doEveryFold(points, instructions)

    Range(0, result.maxBy(_.x).x, 5)
      .foldLeft(List[Set[Point]]()) {
        case (acc, n) => acc.appended(result.collect {
          case Point(x, y) if x >= n && x < n + 5 => Point(x - n, y)
        })
      }
      .map(x => decode(x) match {
        case ' ' => {
          printChar(x)
          ' '
        }
        case y => y
      })
      .mkString
  }
}
