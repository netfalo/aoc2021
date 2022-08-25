package com.github.netfalo.aoc2021

object Day19 extends Problem[Int, Int] {
  case class Beacon(x: Int, y: Int, z: Int) {
    def -(that: Beacon): Beacon = {
      Beacon(x - that.x, y - that.y, z - that.z)
    }

    private def *(that: Beacon): Beacon =
      Beacon(that.x * x, that.y * y, that.z * z)

    def everyOrientation(): Vector[Beacon] = {
      def orientations = Vector(
        Beacon(-1, -1, -1),
        Beacon(1, -1, -1),
        Beacon(-1, 1, -1),
        Beacon(-1, -1, 1),
        Beacon(1, 1, -1),
        Beacon(1, -1, 1),
        Beacon(-1, 1, 1),
        Beacon(1, 1, 1),
      )

      Vector(x, y, z)
        .permutations
        .flatMap { case Vector(_x, _y, _z) => orientations.map(_ * Beacon(_x, _y, _z)) }
        .toVector
    }
  }

  case class Scanner(beacons: Vector[Beacon]) {
    def hasCommonBeacons(that: Scanner): Boolean = {
      getCommonBeacons(that).nonEmpty
    }

    def getCommonBeacons(that: Scanner): Vector[Beacon] = {
      Vector()
    }
  }

  def parseBeacon(row: String): Beacon = row match {
    case s"$x,$y,$z" => Beacon(Integer.parseInt(x), Integer.parseInt(y), Integer.parseInt(z))
  }

  def parseInput(input: String): Vector[Scanner] = {
    val (scanners: Vector[Scanner], Vector()) = input
      .split('\n')
      .drop(1)
      .appended("---")
      .foldLeft((Vector[Scanner](), Vector[Beacon]())) {
        case ((acc, scanner), n) if n.startsWith("---") => (acc.appended(Scanner(scanner)), Vector())
        case (acc, n) if n.isBlank => acc
        case ((acc, scanner), n) => (acc, scanner.appended(parseBeacon(n)))
      }

    scanners
  }

  override def solveFirstPart(input: String): Int = ???

  override def solveSecondPart(input: String): Int = ???
}
