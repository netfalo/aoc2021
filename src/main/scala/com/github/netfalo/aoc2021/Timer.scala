package com.github.netfalo.aoc2021

import com.typesafe.scalalogging.Logger

import java.util.concurrent.TimeUnit
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

  case class PrettyDuration(d: Duration) {
    override def toString: String = {
      @tailrec
      def toString(duration: Duration, str: Vector[String]): String = duration match {
        case d if d.toHours > 0 => toString(d - Duration(d.toHours, TimeUnit.HOURS), str.appended(s"${d.toHours}h"))
        case d if d.toMinutes > 0 => toString(d - Duration(d.toMinutes, TimeUnit.MINUTES), str.appended(s"${d.toMinutes}m"))
        case d if d.toSeconds > 0 => toString(d - Duration(d.toSeconds, TimeUnit.SECONDS), str.appended(s"${d.toSeconds}s"))
        case d if d.toMillis > 0 => toString(d - Duration(d.toMillis, TimeUnit.MILLISECONDS), str.appended(s"${d.toMillis}ms"))
        case d if d.toMicros > 0 => toString(d - Duration(d.toMicros, TimeUnit.MICROSECONDS), str.appended(s"${d.toMicros}us"))
        case d if d.toNanos > 0 => toString(d - Duration(d.toNanos, TimeUnit.NANOSECONDS), str.appended(s"${d.toNanos}ns"))
        case _ => str mkString " "
      }

      toString(d, Vector())
    }
  }

  def timeN[R](block: => R, n: Int): R = {
    timeN("", block, n)
  }

  def timeN[R](prefix: String, block: => R, n: Int): R = {
    val (result, avg) = timeNX(block, n)
    val p = if (prefix.isBlank) "" else prefix + " "
    log.info(s"${p}${if (p.isEmpty) "A" else "a"}verage execution time after $n executions: $avg")
    result
  }

  def timeNX[R](block: => R, n: Int): (R, PrettyDuration) = {
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

    val avg = durations.sum * 1.0 / durations.length
    (result, PrettyDuration(Duration(avg, NANOSECONDS)))
  }
}
