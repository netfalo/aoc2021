package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Timer.timeNX

object TimerApp extends App {
  List(Day16)
    .map { day =>
      val name = day.getClass.getSimpleName.split('$').head
      (name, day, Resource(s"$name.txt").content)
    }
    .map {
      case (name, day, input) =>
        println(name)
        (name.dropWhile(_.isLetter),
          timeNX(day.solveFirstPart(input), 1000)._2,
          timeNX(day.solveSecondPart(input), 1000)._2)
    }
    .map { case (name, first, second) => s"|$name|$first|$second|" }
    .prepended("|----|----|----|")
    .prepended("|Day| part 1| part 2|")
    .foreach(println)
}
