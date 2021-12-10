package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day4 extends Problem[Int] {

  sealed abstract class Cell(number: String) {
    def contains(that: String): Boolean = number == that
  }

  case class Free(number: String) extends Cell(number)

  case class Crossed(number: String) extends Cell(number)

  case class Board(rows: Vector[Vector[Cell]]) {
    def crossOut(number: String): (Board, Boolean) = {
      val newRows = rows
        .map(_.map {
          case Free(d) if d == number => Crossed(d)
          case x => x
        })

      (Board(newRows), newRows.exists(_.forall(_.isInstanceOf[Crossed])) || newRows.transpose.exists(_.forall(_.isInstanceOf[Crossed])))
    }

    def cardScore(): Int = {
      rows
        .flatten
        .map {
          case Free(x) => Integer.parseInt(x)
          case _ => 0
        }
        .sum
    }
  }

  case class Game(drawNumbers: Vector[String], boards: Vector[Board]) {
    @tailrec
    private def playAllGames(winning: Vector[(Board, String)], numbers: Vector[String], boards: Vector[Board]): Vector[(Board, String)] = {
      val grouped = boards
        .map(_.crossOut(numbers.head))
        .groupBy(_._2)
      val solved = if (grouped.contains(true))
        winning.appendedAll(grouped(true).map(m => (m._1, numbers.head)))
      else winning

      if (!grouped.contains(false)) {
        solved
      } else {
        val loosing = grouped(false).map(_._1)

        playAllGames(solved, numbers.tail, loosing)
      }
    }


    def playToWin(): Int = {
      val (board, lastDrawn) = playAllGames(Vector(), drawNumbers, boards).head

      Integer.parseInt(lastDrawn) * board.cardScore()
    }

    def playToLoose(): Int = {
      val (board, lastDrawn) = playAllGames(Vector(), drawNumbers, boards).last

      Integer.parseInt(lastDrawn) * board.cardScore()
    }

  }

  def parseBoard(rows: Array[String]): Board = {
    val r = rows
      .filterNot(_.isBlank)
      .map(_.sliding(3, 3).map(_.replaceAll(" ", "")).map(Free).toVector)
      .toVector

    Board(r)
  }

  def parseGame(input: String): Game = {
    val lines = input.split('\n')
    val drawNumbers = lines
      .dropWhile(_.isBlank)
      .head
      .split(',')
      .toVector

    val boards = lines
      .drop(2)
      .sliding(6, 6)
      .map(parseBoard)
      .toVector

    Game(drawNumbers, boards)
  }

  override def solveFirstPart(input: String): Int = parseGame(input).playToLoose()

  override def solveSecondPart(input: String): Int = parseGame(input).playToLoose()
}
