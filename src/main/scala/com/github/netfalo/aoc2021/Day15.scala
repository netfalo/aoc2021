package com.github.netfalo.aoc2021

import scala.annotation.tailrec
import scala.io.AnsiColor._

object Day15 extends Problem[Int, Int] {
  def parseInput(input: String): (Map[Point, Int], Map[Point, List[Point]], Point, Point) = {
    val lines = input
      .split('\n')
      .filterNot(_.isBlank)

    val points = lines
      .zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.map { case (c, x) => (Point(x, y), c - '0') } }
      .toMap

    val neighbours =
      points
        .map {
          case (p, _) =>
            (p, List(Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1))
              .map(_ + p)
              .filter(neighbour => neighbour.x >= 0 && neighbour.y >= 0 && neighbour.x < lines(0).length && neighbour.y < lines.length)
            )
        }

    (points, neighbours, Point(0, 0), Point(lines(0).length - 1, lines.length - 1))
  }


  def extend(points: Map[Point, Int]): (Map[Point, Int], Map[Point, List[Point]], Point, Point) = {
    val width = points.keys.maxBy(_.x).x + 1
    val height = points.keys.maxBy(_.y).y + 1

    val extendedPoints: Map[Point, Int] = points
      .flatMap {
        case (point@Point(x, y), cost) =>
          (point, cost) +: LazyList
            .from(1)
            .takeWhile(_ <= 4)
            .map(n =>
              (Point(x + width * n, y),
                if ((cost + n) > 9) (cost + n) % 10 + 1
                else cost + n)
            )
      }
      .flatMap {
        case (point@Point(x, y), cost) =>
          (point, cost) +: LazyList
            .from(1)
            .takeWhile(_ <= 4)
            .map(n =>
              (Point(x, y + height * n),
                if ((cost + n) > 9) (cost + n) % 10 + 1
                else cost + n)
            )
      }

    val neighbours = extendedPoints
      .map(p => (p._1, List(Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1))
        .map(_ + p._1)
        .filter(neighbour => neighbour.x >= 0 && neighbour.y >= 0 && neighbour.x < width * 5 && neighbour.y < height * 5)
      ))

    (extendedPoints, neighbours, Point(0, 0), Point(extendedPoints.maxBy(_._1.x)._1.x, extendedPoints.maxBy(_._1.y)._1.y))
  }

  def calculateShortestPathFrom(from: Point, points: Map[Point, Int], neighbours: Map[Point, List[Point]]): (Map[Point, Int], Map[Point, Point]) = {
    @tailrec
    def rec(toVisit: List[Point], allPathCost: Map[Point, Int], lastStepTo: Map[Point, Point]): (Map[Point, Int], Map[Point, Point]) = {
      if (toVisit.isEmpty) (allPathCost, lastStepTo)
      else {
        val current = toVisit.head
        val pointCost = points(current)
        val (minNeighbourCost, neighbour) = neighbours(current).collect { case neighbour if allPathCost.contains(neighbour) => (allPathCost(neighbour), neighbour) }.minBy(_._1)
        val minPathCost = allPathCost.getOrElse(current, pointCost + minNeighbourCost)
        val currentMin = allPathCost.getOrElse(current, Integer.MAX_VALUE)
        if (minPathCost < currentMin)
          rec(toVisit.tail ++ neighbours(current), allPathCost.updated(current, minPathCost), lastStepTo.updated(current, neighbour))
        else
          rec(toVisit.tail, allPathCost, lastStepTo)
      }
    }

    rec(neighbours(from), Map(from -> 0), Map())
  }

  def shortestPathAsList(shortestPath: Map[Point, Point]): List[Point] = {
    val end = Point(shortestPath.maxBy(_._1.x)._1.x, shortestPath.maxBy(_._1.y)._1.y)

    @tailrec
    def getShortestPathTo(point: Point, path: List[Point]): List[Point] = {
      val newPath = path
        .prepended(point)

      if (point == Point(0, 0)) {
        newPath
      } else {
        getShortestPathTo(shortestPath(point), newPath)
      }
    }

    getShortestPathTo(end, List())
  }

  def printMapWithShortestRoute(points: Map[Point, Int], shortestPath: Map[Point, Point]): Unit = {
    val end = Point(points.maxBy(_._1.x)._1.x, points.maxBy(_._1.y)._1.y)

    val shortestPathList = shortestPathAsList(shortestPath)

    println(shortestPathList.tail.map(points).sum)

    Range.inclusive(0, end.y)
      .foreach(y => {
        Range.inclusive(0, end.x)
          .foreach(x =>
            if (shortestPathList.contains(Point(x, y)))
              print(s"${RED}${points(Point(x, y))}${RESET}")
            else
              print(s"${points(Point(x, y))}")
          )
        println
      })
  }

  override def solveFirstPart(input: String): Int = {
    val (points, neighbours, start, end) = parseInput(input)

    val (costs, paths) = calculateShortestPathFrom(start, points, neighbours)

    costs(end)
  }

  override def solveSecondPart(input: String): Int = {
    val (points, neighbours, start, end) = extend(parseInput(input)._1)

    val (costs, paths) = calculateShortestPathFrom(start, points, neighbours)

    costs(end)
  }
}
