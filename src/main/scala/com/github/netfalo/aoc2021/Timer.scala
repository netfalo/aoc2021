package com.github.netfalo.aoc2021

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, NANOSECONDS}

object Timer {
  private val log = Logger("Timer")

  def time[R](block: => R): R = {
    val (result, duration) = timeP(block)
    log.info(s"Elapsed time: ${Duration(duration, NANOSECONDS)}")
    result
  }

  private def timeP[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  def timeN[R](block: => R, n: Int): R = {
    @tailrec
    def timeNRec(block: => R, n: Int, durations: List[Long]): (R, List[Long]) = {
      val (result, duration) = timeP(block)
      if (n > 1) {
        timeNRec(block, n - 1, durations.appended(duration))
      } else {
        (result, durations.appended(duration))
      }
    }
    val (result, durations) = timeNRec(block, n, List())
    log.info(s"Average execution time after $n executions: ${Duration(durations.sum * 1.0 / durations.length, NANOSECONDS)}")
    result
  }
}
