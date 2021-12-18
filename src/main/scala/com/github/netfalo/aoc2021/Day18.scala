package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day18 extends Problem[Int, Int] {

  trait Tree {
    def +(that: Tree): Tree = {
      Node(this, that)
    }

  }

  case class Node(left: Tree, right: Tree) extends Tree {
    override def toString: String = "[" + left.toString + "," + right.toString + "]"
  }

  case class Leaf(value: Int) extends Tree {
    override def toString: String = value.toString
  }

  def findSplit(n: String): Int = {
    @tailrec
    def rec(n: String, cnt: Int, left: Int, right: Int): Int = {
      if (n.head == ',' && left == right) cnt
      else {
        n.head match {
          case '[' => rec(n.tail, cnt + 1, left + 1, right)
          case ']' => rec(n.tail, cnt + 1, left, right + 1)
          case _ => rec(n.tail, cnt + 1, left, right)
        }
      }
    }

    rec(n, 0, 0, 0)
  }

  private val pairPattern = "\\[(.+)\\]".r
  private val numberPattern = "(\\d+)".r

  def parseLine(input: String): Tree = input match {
    case pairPattern(leftRight) =>
      val split = findSplit(leftRight)
      val (left, right) = leftRight.splitAt(split)
      Node(parseLine(left), parseLine(right.tail))
    case numberPattern(value) => Leaf(Integer.parseInt(value))
  }

  def split(tree: Tree): Tree = tree match {
    case Leaf(value) if value >= 10 => Node(Leaf(value / 2), Leaf(Math.ceil(value / 2.0).toInt))
    case leaf: Leaf => leaf
    case node@Node(left, right) =>
      val leftSplitted = split(left)
      if (left != leftSplitted)
        Node(leftSplitted, right)
      else {
        val rightSplitted = split(right)
        if (right != rightSplitted) {
          Node(left, rightSplitted)
        } else {
          node
        }
      }
  }

  def addToLeftMost(tree: Day18.Tree, n: Int): Tree = tree match {
    case Leaf(value) => Leaf(value + n)
    case Node(left, right) => Node(addToLeftMost(left, n), right)
  }

  def addToRightMost(tree: Day18.Tree, n: Int): Tree = tree match {
    case Leaf(value) => Leaf(value + n)
    case Node(left, right) => Node(left, addToRightMost(right, n))
  }

  def explode(tree: Tree): Tree = {
    def rec(tree: Tree, depth: Int): (Tree, Option[Tree]) = tree match {
      case node@Node(Leaf(_), Leaf(_)) if depth > 4 => (Leaf(0), Some(node))
      case node@Node(Leaf(_), Leaf(_)) => (node, None)
      case node@Node(left, right) =>
        val (leftReduced, remainder) = rec(left, depth + 1)
        if (leftReduced != left) {
          if (remainder.isDefined) {
            val (v, r) = remainder match {
              case Some(Node(Leaf(r), Leaf(value))) => (value, Some(Node(Leaf(r), Leaf(0))))
            }
            (Node(leftReduced, addToLeftMost(right, v)), r)
          } else {
            (Node(leftReduced, right), None)
          }
        }
        else {
          val (rightReduced, remainder) = rec(right, depth + 1)
          if (rightReduced != right) {
            if (remainder.isDefined) {
              val (v, r) = remainder match {
                case Some(Node(Leaf(value), Leaf(r))) => (value, Some(Node(Leaf(0), Leaf(r))))
              }

              (Node(addToRightMost(left, v), rightReduced), r)
            }
            else {
              (Node(left, rightReduced), None)
            }
          }
          else (node, None)
        }
      case leaf: Leaf => (leaf, None)
    }

    val (updated, _) = rec(tree, 1)

    updated
  }

  @tailrec
  def reduce(tree: Tree): Tree = {
    val exploded = explode(tree)
    if (exploded != tree) {
      reduce(exploded)
    } else {
      val splitted = split(tree)
      if (splitted != tree) {
        reduce(splitted)
      } else {
        tree
      }
    }
  }

  def magnitude(tree: Tree): Int = tree match {
    case Leaf(value) => value
    case Node(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
  }

  def parseInput(input: String): Vector[Tree] = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(parseLine)
      .toVector
  }

  override def solveFirstPart(input: String): Int = {
    val lines = parseInput(input)
    val sum = lines
      .tail
      .foldLeft(lines.head)((acc, b) => reduce(acc + b))

    magnitude(sum)
  }

  override def solveSecondPart(input: String): Int = {
    val lines = parseInput(input)

    (lines
      .combinations(2)
      ++
      lines
        .reverse
        .combinations(2))
      .distinct
      .map { case Vector(left, right) => reduce(left + right) }
      .map(sum => magnitude(sum))
      .max
  }
}
