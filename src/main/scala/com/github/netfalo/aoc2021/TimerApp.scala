package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Timer.timeNX

object TimerApp extends App {
  val tasks = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10)
    .map(x =>
      (x,
       Resource(s"${x.getClass.getSimpleName.filterNot('$' == _)}.txt").content))

  val table = tasks
    .map(
      x =>
        (x._1.getClass.getSimpleName.filterNot('$' == _).replaceAll("Day", ""),
         timeNX(x._1.solveFirstPart(x._2), 1000)._2,
         timeNX(x._1.solveSecondPart(x._2), 1000)._2))
    .map { case (name, first, second) => s"|$name|$first|$second|" }
    .prepended("|----|----|----|")
    .prepended("|Day| part 1| part 2|")

  table
    .foreach(println)
}
