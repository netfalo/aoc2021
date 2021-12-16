package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day14 extends Problem[Long, Long] {

  def parseInput(input: String): (Map[String, Long], Map[Char, Long], Map[String, String]) = {
    val lines = input.split('\n')
    val rules = lines
      .drop(2)
      .map { case s"$pair -> $inserted" => (pair, inserted) }
      .toMap
    val pairs = lines.head
      .sliding(2)
      .toList
      .map((_, 1L))
      .groupMapReduce(_._1)(_._2)(_ + _)
    val count = lines.head
      .map((_, 1L))
      .groupMapReduce(_._1)(_._2)(_ + _)

    (pairs, count, rules)
  }

  def doOneInsertion(polymer: Map[String, Long], rules: Map[String, String], counter: Map[Char, Long]): (Map[String, Long], Map[Char, Long]) = {
    val (pairs, counts) = polymer
      .toList
      .map {
        case (pair, count) =>
          val x = rules(pair)
          val left = pair.head + x
          val right = x + pair.last
          (List((pair, -count), (left, count), (right, count)), (x.head, count))
      }
      .foldLeft((List[(String, Long)](), List[(Char, Long)]())) { case ((pairs, counts), n) => (pairs ++ n._1, counts.appended(n._2)) }

    (
      (pairs ++ polymer)
        .groupMapReduce(_._1)(_._2)(_ + _)
        .filter(_._2 > 0),
      (counts ++ counter)
        .groupMapReduce(_._1)(_._2)(_ + _)
    )
  }

  def doNInsertion(polymer: Map[String, Long], rules: Map[String, String], counter: Map[Char, Long], n: Int): Map[Char, Long] = {
    LazyList
      .from(0)
      .take(n)
      .foldLeft((polymer, counter)) { case ((p, c), _) => doOneInsertion(p, rules, c) }
      ._2
  }

  override def solveFirstPart(input: String): Long = {
    val (pairs, originalCount, rules) = parseInput(input)

    val counts = doNInsertion(pairs, rules, originalCount, 10)

    counts.values.max - counts.values.min
  }

  override def solveSecondPart(input: String): Long = {
    val (pairs, originalCount, rules) = parseInput(input)

    val counts = doNInsertion(pairs, rules, originalCount, 40)

    counts.values.max - counts.values.min
  }
}
