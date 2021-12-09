package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day3 extends Problem {

  type ReportLine = IndexedSeq[Int]
  type Report = IndexedSeq[ReportLine]

  def parseReport(input: String): Report = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .map(_.map(_.toInt - '0'))
  }

  def calculateGammaRate(input: Report): Int = {
    val gammaRateBin = input
      .transpose
      .map(r => if (r.sum > input.size / 2) 1 else 0)
      .mkString

    Integer.parseInt(gammaRateBin, 2)
  }

  override def solveFirstPart(input: String): String = {
    val report = parseReport(input)
    val gammaRate = calculateGammaRate(report)

    (gammaRate * (gammaRate ^ (Math.pow(2, report(0).size).toInt - 1)))
      .toString
  }

  def findRating(report: Report, bitCriteria: (Int, Int) => Int): Int = {
    @tailrec
    def findRating(report: Report, bitCriteria: (Int, Int) => Int, bitPosition: Int): Int = {
      if (report.size == 1)
        Integer.parseInt(report.head.mkString, 2)
      else
        findRating(filterByBitCriteria(report, bitCriteria, bitPosition), bitCriteria, bitPosition + 1)
    }

    findRating(report, bitCriteria, 0)
  }

  def filterByBitCriteria(report: Report, bitCriteria: (Int, Int) => Int, bitPosition: Int): Report = {
    val numberOfOnes = report
      .map(_ (bitPosition))
      .sum
    val mostCommonBit = bitCriteria(numberOfOnes, report.size)

    report
      .filter(_ (bitPosition) == mostCommonBit)
  }

  def getOxygenGeneratorRating(report: Report): Int =
    findRating(report, (numberOfOnes, reportSize) => if (numberOfOnes >= reportSize / 2.0) 1 else 0)

  def getCo2ScrubbingRating(report: Report): Int =
    findRating(report, (numberOfOnes, reportSize) => if (numberOfOnes >= reportSize / 2.0) 0 else 1)

  override def solveSecondPart(input: String): String = {
    val report = parseReport(input)
    val oxygenGeneratorRating = getOxygenGeneratorRating(report)
    val co2ScrubbingRating = getCo2ScrubbingRating(report)

    (oxygenGeneratorRating * co2ScrubbingRating)
      .toString
  }

}
