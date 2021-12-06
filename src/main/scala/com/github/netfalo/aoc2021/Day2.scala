package com.github.netfalo.aoc2021

object Day2 extends Problem {

  sealed trait Direction

  case class Down(distance: Int) extends Direction

  case class Forward(distance: Int) extends Direction

  case class Position(depth: Int, horizontal: Int) {
    def +(direction: Direction): Position = {
      direction match {
        case Down(x) => Position(depth + x, horizontal)
        case Forward(x) => Position(depth, horizontal + x)
      }
    }

    def size: Int = depth * horizontal
  }

  case class PositionWithAim(depth: Int, horizontal: Int, aim: Int) {
    def +(command: Direction): PositionWithAim =
        command match {
          case Down(x) => PositionWithAim(depth, horizontal, aim + x)
          case Forward(x) => PositionWithAim(depth + aim * x, horizontal + x, aim)
        }

    def size: Int = depth * horizontal
  }


  private val startPosition = Position(0, 0)
  private val startPositionWithAim = PositionWithAim(0, 0, 0)

  def parse(command: String): Direction = {
    val forward = "forward (\\d+)".r
    val down = "down (\\d+)".r
    val up = "up (\\d+)".r
    command match {
      case forward(x) => Forward(x.toInt)
      case down(x) => Down(x.toInt)
      case up(x) => Down(-x.toInt)
      case _ => throw new RuntimeException(s"'$command' doesn't match anything")
    }
  }

  override def solveFirstPart(input: String): String = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(parse)
      .foldLeft(startPosition)(_ + _)
      .size
      .toString
  }

  override def solveSecondPart(input: String): String = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(parse)
      .foldLeft(startPositionWithAim)(_ + _)
      .size
      .toString
  }

}
