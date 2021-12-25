package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day21 extends Problem[Long, Long] {
  def parseInput(input: String): (Int, Int) = {
    val lines = input
      .split('\n')
      .filterNot(_.isBlank)

    assert(lines.length == 2)

    val s"Player 1 starting position: $playerOne" = lines.head
    val s"Player 2 starting position: $playerTwo" = lines(1)

    (Integer.parseInt(playerOne), Integer.parseInt(playerTwo))
  }

  case class Dice(next: Int, cnt: Int) {
    def roll(): (Int, Dice) = {
      val nn = if (next == 100) 1
      else next + 1

      (next, new Dice(nn, cnt + 1))
    }

    def roll(n: Int): (Int, Dice) = {
      Range(0, n)
        .foldLeft((0, this)) {
          case ((sum, dice), _) => {
            val (rolled, d) = dice.roll()
            (rolled + sum, d)
          }
        }
    }
  }

  trait Player

  object PlayerOne extends Player

  object PlayerTwo extends Player

  object Dice {
    def apply(): Dice = new Dice(1, 0)
  }


  implicit class VectorExtension[T](it: Vector[T]) {
    private def combinationsRepeating(n: Int): Vector[Vector[T]] = {
      if (n == 1) it.map(Vector(_))
      else {
        val x = combinationsRepeating(n - 1)
        it.flatMap(n => x.map(_.prepended(n)))
      }
    }

    def combinationsRepeating(): Vector[Vector[T]] = {
      combinationsRepeating(3)
    }
  }

  private val splits = Vector(1, 2, 3)
    .combinationsRepeating()
    .map(_.sum)

  case class Game(limit: Int, playerOnePosition: Int, playerTwoPosition: Int, playerOneScore: Int, playerTwoScore: Int, playerOneTurn: Boolean) {
    def isFinished: Boolean =
      limit <= playerOneScore || limit <= playerTwoScore

    def result: (Long, Long) =
      if (limit <= playerOneScore)
        (1L, 0L)
      else
        (0L, 1L)

    def play(): Vector[Game] = {
      if (playerOneTurn) {
        splits
          .map(roll => {
            val newPosition = if ((playerOnePosition + roll) % 10 == 0) 10 else (playerOnePosition + roll) % 10
            Game(limit, newPosition, playerTwoPosition, playerOneScore + newPosition, playerTwoScore, playerOneTurn = false)
          })
      } else {
        splits
          .map(roll => {
            val newPosition = if ((playerTwoPosition + roll) % 10 == 0) 10 else (playerTwoPosition + roll) % 10
            Game(limit, playerOnePosition, newPosition, playerOneScore, playerTwoScore + newPosition, playerOneTurn = true)
          })
      }
    }
  }

  def play(dice: Dice, score: Int, position: Int): (Dice, Int, Int) = {
    val (roll, d1) = dice.roll(3)
    val newPosition = if ((position + roll) % 10 == 0) 10 else (position + roll) % 10

    (d1, score + newPosition, newPosition)
  }

  @tailrec
  def playUntilScore(dice: Dice, value: Int, playerOneScore: Int, playerOnePosition: Int, playerTwoScore: Int, playerTwoPosition: Int): Int = {
    val (d1, playerOneNewScore, playerOneNewPosition) = play(dice, playerOneScore, playerOnePosition)
    if (playerOneNewScore >= value)
      return playerTwoScore * d1.cnt

    val (d2, playerTwoNewScore, playerTwoNewPosition) = play(d1, playerTwoScore, playerTwoPosition)
    if (playerTwoNewScore >= value)
      playerOneNewScore * d2.cnt
    else
      playUntilScore(d2, value, playerOneNewScore, playerOneNewPosition, playerTwoNewScore, playerTwoNewPosition)

  }

  private lazy val occurrence = Vector(1, 2, 3)
    .combinationsRepeating()
    .map(_.sum)
    .groupBy(identity)
    .map { case (xs, ys) => (xs, ys.size) }


  override def solveFirstPart(input: String): Long = {
    val (playerOne, playerTwo) = parseInput(input)

    playUntilScore(Dice(), 1000, 0, playerOne, 0, playerTwo)
  }

  override def solveSecondPart(input: String): Long = {
    val (playerOne, playerTwo) = parseInput(input)

    println(occurrence)
    0L
  }
}
