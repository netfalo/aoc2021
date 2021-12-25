package com.github.netfalo.aoc2021

object Day22 extends Problem[Long, Long] {

  case class CoordinateRange(from: Int, to: Int)

  case class Command(value: Int, xs: CoordinateRange, ys: CoordinateRange, zs: CoordinateRange)

  def parseInput(input: String): Vector[Command] = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .foldLeft(Vector[Command]()) {
        case (acc, s"$command x=$x1..$x2,y=$y1..$y2,z=$z1..$z2") => acc.appended(Command(command match {
          case "on" => 1
          case "off" => 0
        }, CoordinateRange(Integer.parseInt(x1), Integer.parseInt(x2)), CoordinateRange(Integer.parseInt(y1), Integer.parseInt(y2)), CoordinateRange(Integer.parseInt(z1), Integer.parseInt(z2))))
      }
  }

  private val initialCubes = Vector.fill(101)(Vector.fill(101)(Vector.fill(101)(0)))

  def runCommand(cubes: Vector[Vector[Vector[Int]]], command: Command): Vector[Vector[Vector[Int]]] = {
    val Command(c, CoordinateRange(x1, x2), CoordinateRange(y1, y2), CoordinateRange(z1, z2)) = command
    val xs = Range.inclusive(Math.max(x1, -50) + 50, Math.min(x2, 50) + 50)
    val ys = Range.inclusive(Math.max(y1, -50) + 50, Math.min(y2, 50) + 50)
    val zs = Range.inclusive(Math.max(z1, -50) + 50, Math.min(z2, 50) + 50)

    Range.inclusive(0, 100)
      .map(x => Range.inclusive(0, 100)
        .map(y => Range.inclusive(0, 100)
          .map(z => if (xs.contains(x) && ys.contains(y) && zs.contains(z)) c
          else cubes(x)(y)(z)
          )
          .toVector)
        .toVector)
      .toVector
  }

  override def solveFirstPart(input: String): Long = {
    val commands = parseInput(input)

    commands
      .foldLeft(initialCubes)(runCommand)
      .map(_.map(_.sum).sum)
      .sum
  }

  override def solveSecondPart(input: String): Long = {
    val commands = parseInput(input)
    ???
  }
}
