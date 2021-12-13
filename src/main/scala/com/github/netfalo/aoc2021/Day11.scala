package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day11 extends Problem[Long, Long] {

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = {
      Point(that.x + x, that.y + y)
    }
  }

  case class Grid(octopuses: Map[Point, Int], neighbours: Map[Point, Seq[Point]], gridWidth: Int, gridHeight: Int) {
    override def toString: String = {
      val ys = Range(0, gridHeight)
      val xs = Range(0, gridWidth)
      ys
        .map(y => xs.map(Point(_, y)))
        .map(_.map(octopuses).mkString)
        .mkString("\n")
    }

    def runFlashes(octopuses: Map[Point, Int]): Map[Point, Int] = {
      if (octopuses.forall { case (_, v) => v <= 9 }) {
        octopuses
      } else {
        val updated = octopuses
          .map {
            case (p, v) if v > 0 && v <= 9 => {
              val activeNeighbours = neighbours(p)
                .map(octopuses)
                .count(_ > 9)
              (p, v + activeNeighbours)
            }
            case (p, v) if v > 9 => (p, 0)
            case (p, 0) => (p, 0)
          }

        runFlashes(updated)
      }
    }

    def step(octopuses: Map[Point, Int]): Map[Point, Int] = {
      val increasedByOne = octopuses.map { case (p, v) => (p, v + 1) }
      runFlashes(increasedByOne)
    }

    def stepN(n: Int): (Map[Point, Int], Int) =
      LazyList
        .from(0)
        .take(n)
        .foldLeft((octopuses, 0)) {
          case ((octopuses, flashes), _) =>
            val updated = step(octopuses)
            (updated, flashes + updated.values.count(_ == 0))
        }

    def firstSynchronizedFlash(): Long =
    {
      @tailrec
      def rec(octopuses: Map[Point, Int], acc: Long): Long = {
        if (octopuses.values.forall(_ == 0))
          acc
        else rec(step(octopuses), acc + 1)
      }

      rec(this.octopuses, 0)
    }
  }

  def getNeighbours(point: Point, mapWidth: Int, mapHeight: Int): Seq[Point] = {
    Seq(
      Point(-1, -1), Point(-1, 0), Point(-1, 1),
      Point(0, -1), Point(0, 0), Point(0, 1),
      Point(1, -1), Point(1, 0), Point(1, 1)
    )
      .map(_ + point)
      .filter(_p => _p.x >= 0 && _p.y >= 0 && _p.x < mapWidth && _p.y < mapHeight)
  }

  def parseGrid(input: String): Grid = {
    val lines =
      input
        .split('\n')
        .filterNot(_.isBlank)

    val points = lines
      .zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.map {
        case (c, x) => (Point(x, y), c - '0')
      }
      }.toMap
    val mapWidth = lines.head.length
    val mapHeight = lines.length
    val neighbours = points.keys
      .map(point => (point, getNeighbours(point, mapWidth, mapHeight)))
      .toMap

    Grid(points, neighbours, mapWidth, mapHeight)
  }

  override def solveFirstPart(input: String): Long =
    parseGrid(input).stepN(100)._2

  override def solveSecondPart(input: String): Long =
    parseGrid(input).firstSynchronizedFlash()
}
