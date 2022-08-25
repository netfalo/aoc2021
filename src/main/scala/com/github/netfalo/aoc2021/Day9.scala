package com.github.netfalo.aoc2021

object Day9 extends Problem[Int, Int] {

  case class HeatMap(rows: Vector[Vector[Int]]) {
    private val mapHeight = rows.length
    private val mapWidth = rows(0).length

    def totalRiskLevel: Int = localMinimums
      .map { case (x, y) => rows(x)(y) }
      .foldLeft(0)((acc, x) => acc + x + 1)

    def localMinimums: Vector[(Int, Int)] = {
      Range(0, mapHeight)
        .flatMap(x => Range(0, mapWidth).map((x, _)))
        .filter { case (x, y) => isLocalMin(x, y) }
        .toVector
    }

    def neighbours(x: Int, y: Int): Vector[(Int, Int)] = {
      Vector((-1, 0), (0, -1), (1, 0), (0, 1))
        .map { case (_x, _y) => (x + _x, y + _y) }
        .filter { case (_x, _y) => _x >= 0 && _y >= 0 && _x < mapHeight && _y < mapWidth }
    }

    private def isLocalMin(x: Int, y: Int): Boolean =
      neighbours(x, y)
        .map { case (_x, _y) => rows(_x)(_y) }
        .forall(_ > rows(x)(y))

    def basin(x: Int, y: Int): Vector[(Int, Int)] = {
      neighbours(x, y)
        .filter { case (_x, _y) => rows(x)(y) < rows(_x)(_y) && rows(_x)(_y) != 9 }
        .flatMap { case (x, y) => basin(x, y) }
        .appended((x, y))
        .distinct
    }

    def sumBasinAreas(): Int = localMinimums
      .map { case (x, y) => basin(x, y) }
      .map(_.size)
      .sorted
      .reverse
      .take(3)
      .product
  }

  def parseInput(example: String): HeatMap = {
    val rows = example
      .split('\n')
      .filterNot(_.isBlank)
      .map(_.map(_ - '0').toVector)
      .toVector

    HeatMap(rows)
  }

  override def solveFirstPart(input: String): Int =
    parseInput(input).totalRiskLevel

  override def solveSecondPart(input: String): Int =
    parseInput(input).sumBasinAreas()
}
