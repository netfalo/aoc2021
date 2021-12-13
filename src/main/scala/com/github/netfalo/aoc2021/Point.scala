package com.github.netfalo.aoc2021

case class Point(x: Int, y: Int) {
  def +(that: Point): Point = {
    Point(that.x + x, that.y + y)
  }

  def mirror(axis: Point): Point = axis match {
    case Point(_x, 0) =>
      if (x < _x)
        this
      else
        Point(2 * _x - x, y)
    case Point(0, _y) =>
      if (y < _y)
        this
      else
        Point(x, 2 * _y - y)
  }
}
