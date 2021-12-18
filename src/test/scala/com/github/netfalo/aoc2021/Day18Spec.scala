package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day18._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {
  private val example =
    """
      |[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
      |""".stripMargin

  private lazy val exercise = Resource("Day18.txt").content

  "Day18" should "parse" in {
    val expected = Node(
      Node(
        Node(
          Node(Leaf(1), Leaf(3)),
          Node(Leaf(5), Leaf(3))),
        Node(
          Node(Leaf(1), Leaf(3)),
          Node(Leaf(8), Leaf(7)))
      ),
      Node(
        Node(
          Node(Leaf(4), Leaf(9)),
          Node(Leaf(6), Leaf(9))),
        Node(
          Node(Leaf(8), Leaf(2)),
          Node(Leaf(7), Leaf(3)))
      )
    )
    Day18.parseLine("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]") shouldEqual expected
  }

  it should "calculate magnitude" in {
    magnitude(parseLine("[[[[5,0],[7,4]],[5,5]],[6,6]]")) shouldEqual 1137
    magnitude(parseLine("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) shouldEqual 3488
    magnitude(parseLine("[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]")) shouldEqual 3993
  }

  it should "split" in {
    split(parseLine("[[[[0,7],4],[15,[0,13]]],[1,1]]")) shouldEqual parseLine("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
    split(parseLine("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")) shouldEqual parseLine("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
  }

  it should "explode" in {
    explode(parseLine("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")) shouldEqual parseLine("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")
    explode(parseLine("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")) shouldEqual parseLine("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    explode(parseLine("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")) shouldEqual parseLine("[[[[0,7],4],[15,[0,13]]],[1,1]]")
  }

  it should "reduce" in {
    reduce(parseLine("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")) shouldEqual parseLine("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    reduce(parseLine("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]") + parseLine("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]")) shouldEqual parseLine("[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]")
  }

  it should "solve first part" in {
    solveFirstPart(example) shouldEqual 4140
    solveFirstPart(exercise) shouldEqual 3699
  }

  it should "solve second part" in {
    solveSecondPart(example) shouldEqual 3993
    solveSecondPart(exercise) shouldEqual 4735
  }
}
