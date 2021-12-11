package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day11 extends Problem[Long] {

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = {
      Point(that.x + x, that.y + y)
    }
  }

  case class Grid(octopuses: Map[Point, Int], gridWidth: Int, gridHeight: Int) {
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
              val activeNeighbours = List(
                Point(-1, -1), Point(-1, 0), Point(-1, 1),
                Point(0, -1), Point(0, 0), Point(0, 1),
                Point(1, -1), Point(1, 0), Point(1, 1)
              )
                .map(_ + p)
                .filter(_p => _p.x >= 0 && _p.y >= 0 && _p.x < gridWidth && _p.y < gridHeight)
                .map(octopuses(_))
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

    def stepN(n: Int): (Grid, Long) = {
      @tailrec
      def stepRec(n: Int, octopuses: Map[Point, Int], flashes: Long): (Grid, Long) = {
        if (n == 0)
          (Grid(octopuses, gridWidth, gridHeight), flashes)
        else {
          val updated = step(octopuses)
          stepRec(n - 1, updated, flashes + updated.values.count(_ == 0))
        }
      }

      stepRec(n, octopuses, 0)
    }

    def firstSynchronizedFlash(): Long = {
      @tailrec
      def rec(octopuses: Map[Point, Int], acc: Long): Long = {
        if (octopuses.values.forall(_ == 0))
          acc
        else rec(step(octopuses), acc + 1)
      }

      rec(this.octopuses, 0)
    }
  }

  def parseGrid(input: String): Grid = {
    val lines =
      input
        .split('\n')
        .filterNot(_.isBlank)

    Grid(
      lines
        .zipWithIndex
        .flatMap { case (line, y) => line.zipWithIndex.map {
          case (c, x) => (Point(x, y), c - '0')
        }
        }.toMap, lines.head.length, lines.length)
  }

  override def solveFirstPart(input: String): Long = 0L

  override def solveSecondPart(input: String): Long = 0L
}
