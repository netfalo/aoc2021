package com.github.netfalo.aoc2021

import scala.annotation.tailrec
import scala.util.Either

object Day10 extends Problem {
  def parseLines(example: String): Seq[String] =
    example
      .split('\n')
      .filterNot(_.isBlank)

  private val pairs = Map(
    '(' -> ')',
    '{' -> '}',
    '[' -> ']',
    '<' -> '>'
  )

  private def isOpening(c: Char): Boolean = pairs.keySet.contains(c)

  private def isPair(opening: Char, closing: Char): Boolean =
    pairs(opening) == closing

  def isCorrupted(str: String): Either[Vector[Char], Char] = {
    @tailrec
    def isCorrupted(str: String, opened: Vector[Char]): Either[Vector[Char], Char] = {
      if (str.isEmpty)
        Left(opened)
      else
        str.head match {
          case c if isOpening(c) => isCorrupted(str.tail, opened.prepended(c))
          case c if isPair(opened.head, c) => isCorrupted(str.tail, opened.tail)
          case c => Right(c)
        }
    }

    isCorrupted(str, Vector())
  }

  private val corruptionScore: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  override def solveFirstPart(input: String): String =
    parseLines(input)
      .map(x => {
        isCorrupted(x) match {
          case Right(c) => corruptionScore(c)
          case _ => 0
        }
      })
      .sum
      .toString


  private def complete(x: Vector[Char]): Vector[Char] = {
    x.foldLeft(Vector[Char]())((acc, n) => acc.appended(pairs(n)))
  }

  val completionScore: Map[Char, Long => Long] = Map(
    ')' -> (x => x * 5 + 1),
    ']' -> (x => x * 5 + 2),
    '}' -> (x => x * 5 + 3),
    '>' -> (x => x * 5 + 4),
  )

  private def scoreCompletion(x: Vector[Char]): Long = x.foldLeft(0L)((acc, n) => completionScore(n)(acc))

  override def solveSecondPart(input: String): String = {
    val completionScores = parseLines(input)
      .map(isCorrupted)
      .collect {case Left(value) => scoreCompletion(complete(value))}
      .sorted

    completionScores(completionScores.length / 2)
      .toString
  }
}
