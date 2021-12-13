package com.github.netfalo.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {
  private val smallExample =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin
  private val slightlyLargerExample =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin
  private val evenLargerExample =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW
      |
      |""".stripMargin
  private lazy val exercise = Resource("Day12.txt").content

  "parseGraph" should "parse smallExample" in {
    Day12.parseGraph(smallExample) shouldEqual Map(
      "start" -> Seq("A", "b"),
      "c" -> Seq("A"),
      "A" -> Seq("c", "b", "end"),
      "b" -> Seq("A", "d", "end"),
      "d" -> Seq("b"),
      "end" -> Seq("A", "b")
    )
  }

  "visitAllRoutes" should "find all valid route" in {
    val expected = Set(
      List("start", "A", "b", "A", "c", "A", "end"),
      List("start", "A", "b", "A", "end"),
      List("start", "A", "b", "end"),
      List("start", "A", "c", "A", "b", "A", "end"),
      List("start", "A", "c", "A", "b", "end"),
      List("start", "A", "c", "A", "end"),
      List("start", "A", "end"),
      List("start", "b", "A", "c", "A", "end"),
      List("start", "b", "A", "end"),
      List("start", "b", "end")
    )

    Day12.findAllRoutesVisitSmallCavesOnce(Day12.parseGraph(smallExample)) shouldEqual expected
  }

  it should "find all routes for slightlyLarger example" in {
    Day12.findAllRoutesVisitSmallCavesOnce(Day12.parseGraph(slightlyLargerExample)).size shouldEqual 19
  }

  it should "find all routes for evenLarger example" in {
    Day12.findAllRoutesVisitSmallCavesOnce(Day12.parseGraph(evenLargerExample)).size shouldEqual 226
  }

  it should "find all routes for exercise" in {
    Day12.findAllRoutesVisitSmallCavesOnce(Day12.parseGraph(exercise)).size shouldEqual 5920
  }

  it should "find all routes for small when one can be visited twice for smallExample" in {
    val expected = Set(
      Seq("start", "A", "b", "A", "b", "A", "c", "A", "end"),
      Seq("start", "A", "b", "A", "b", "A", "end"),
      Seq("start", "A", "b", "A", "b", "end"),
      Seq("start", "A", "b", "A", "c", "A", "b", "A", "end"),
      Seq("start", "A", "b", "A", "c", "A", "b", "end"),
      Seq("start", "A", "b", "A", "c", "A", "c", "A", "end"),
      Seq("start", "A", "b", "A", "c", "A", "end"),
      Seq("start", "A", "b", "A", "end"),
      Seq("start", "A", "b", "d", "b", "A", "c", "A", "end"),
      Seq("start", "A", "b", "d", "b", "A", "end"),
      Seq("start", "A", "b", "d", "b", "end"),
      Seq("start", "A", "b", "end"),
      Seq("start", "A", "c", "A", "b", "A", "b", "A", "end"),
      Seq("start", "A", "c", "A", "b", "A", "b", "end"),
      Seq("start", "A", "c", "A", "b", "A", "c", "A", "end"),
      Seq("start", "A", "c", "A", "b", "A", "end"),
      Seq("start", "A", "c", "A", "b", "d", "b", "A", "end"),
      Seq("start", "A", "c", "A", "b", "d", "b", "end"),
      Seq("start", "A", "c", "A", "b", "end"),
      Seq("start", "A", "c", "A", "c", "A", "b", "A", "end"),
      Seq("start", "A", "c", "A", "c", "A", "b", "end"),
      Seq("start", "A", "c", "A", "c", "A", "end"),
      Seq("start", "A", "c", "A", "end"),
      Seq("start", "A", "end"),
      Seq("start", "b", "A", "b", "A", "c", "A", "end"),
      Seq("start", "b", "A", "b", "A", "end"),
      Seq("start", "b", "A", "b", "end"),
      Seq("start", "b", "A", "c", "A", "b", "A", "end"),
      Seq("start", "b", "A", "c", "A", "b", "end"),
      Seq("start", "b", "A", "c", "A", "c", "A", "end"),
      Seq("start", "b", "A", "c", "A", "end"),
      Seq("start", "b", "A", "end"),
      Seq("start", "b", "d", "b", "A", "c", "A", "end"),
      Seq("start", "b", "d", "b", "A", "end"),
      Seq("start", "b", "d", "b", "end"),
      Seq("start", "b", "end"))

    val actual = Day12.findAllRoutesVisitOneSmallCaveTwice(Day12.parseGraph(smallExample))

    actual shouldEqual expected
  }

  it should "find all routes for small when one can be visited twice for slightlyLargerExample" in {
    Day12.findAllRoutesVisitOneSmallCaveTwice(Day12.parseGraph(slightlyLargerExample)).size shouldEqual 103
  }

  it should "find all routes for small when one can be visited twice for evenLargerExample" in {
    Day12.findAllRoutesVisitOneSmallCaveTwice(Day12.parseGraph(evenLargerExample)).size shouldEqual 3509
  }

  it should "find all routes for small when one can be visited twice for exercise" in {
    Day12.findAllRoutesVisitOneSmallCaveTwice(Day12.parseGraph(exercise)).size shouldEqual 155477
  }
}
