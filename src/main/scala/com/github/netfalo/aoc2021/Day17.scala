package com.github.netfalo.aoc2021

object Day17 extends Problem[Int, Int] {

  def parseInput(n: String): ((Int, Int), (Int, Int)) = n match {
    case s"target area: x=$x1..$x2, y=$y1..$y2" => ((Integer.parseInt(x1), Integer.parseInt(x2)), (Integer.parseInt(y1), Integer.parseInt(y2)))
  }

  def calculateMaxHeight(y1: Int, y2: Int): Int = {
    val longer = Math.max(Math.abs(y1), Math.abs(y2))
    val shorter = Math.min(Math.abs(y1), Math.abs(y2))
    val yVelocity = LazyList
      .from(1)
      .take(longer + 1)
      .map(v => (v, calculateTimeOfFlightUp(v, shorter), calculateTimeOfFlightUp(v, longer)))
      .filter { case (_, t1, t2) =>
        Math.floor(t2) > t1
      }
      .map(_._1)
      .max

    yVelocity * (yVelocity + 1) / 2
  }

  def calculateTimeOfFlightUp(initialVelocity: Int, height: Int): Double = {
    initialVelocity + Math.sqrt(Math.pow(initialVelocity, 2) + 2 * height)
  }

  def calculateTimeOfFlightDown(initialVelocity: Int, distance: Int): Double = {
    (2 - 2 * initialVelocity + Math.sqrt(Math.pow(2 * initialVelocity - 2, 2) - 4 * (1 - 2 * distance))) / 2
  }

  def calculatePossibleVelocities(x1: Int, x2: Int, y1: Int, y2: Int): Set[(Int, Int)] = {
    val longer = Math.max(Math.abs(y1), Math.abs(y2))
    val shorter = Math.min(Math.abs(y1), Math.abs(y2))
    val possibleVelocityVectors = LazyList
      .from(longer * -1)
      .take(2 * longer + 1)
      .map {
        case v if v >= 0 =>
          (v, calculateTimeOfFlightUp(Math.abs(v), shorter), calculateTimeOfFlightUp(Math.abs(v), longer))
        case v if v < 0 =>
          (v, calculateTimeOfFlightDown(Math.abs(v), shorter), calculateTimeOfFlightDown(Math.abs(v), longer))
      }
      .filter { case (v, t1, t2) =>
        Math.floor(t2) >= t1
      }
      .flatMap {
        case (v, t1, t2) =>
          LazyList
            .from(0)
            .take(x2 + 1)
            .filter {
              case vx if vx < t2.floor =>
                2 * x1 <= vx * vx + vx && vx * vx + vx <= 2 * x2
              case vx =>
                x1 <= ((vx * (vx + 1)) / 2 - (vx - t2.floor) * (vx - t2.floor + 1) / 2) && ((vx * (vx + 1)) / 2 - (vx - t2.floor) * (vx - t2.floor + 1) / 2) <= x2
            }
            .map(vx => (vx, v))
      }
      .toSet

    possibleVelocityVectors
  }

  override def solveFirstPart(input: String): Int = {
    val (_, (y1, y2)) = parseInput(input)

    calculateMaxHeight(y1, y2)
  }

  override def solveSecondPart(input: String): Int = ???
}
