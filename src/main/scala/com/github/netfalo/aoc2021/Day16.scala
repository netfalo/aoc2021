package com.github.netfalo.aoc2021

import scala.annotation.tailrec

object Day16 extends Problem[Int, Long] {

  sealed abstract class Packet(version: Int, typeId: Int) {
    def totalVersionSum: Int = ???

    def value: Long = ???
  }

  case class Literal(version: Int, typeId: Int, content: Long) extends Packet(version, typeId) {
    override def totalVersionSum: Int = version

    override def value: Long = content
  }

  sealed abstract class Operator(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Packet(version, typeId) {
    final override def totalVersionSum: Int = version + content.map(_.totalVersionSum).sum
  }

  case class Sum(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long = content.map(_.value).sum
  }

  case class Product(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long = content.map(_.value).product
  }

  case class Minimum(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long = content.map(_.value).min
  }

  case class Maximum(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long = content.map(_.value).max
  }

  case class GreaterThan(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long =
      if (content(0).value > content(1).value) 1
      else 0
  }

  case class LessThan(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long =
      if (content(0).value < content(1).value) 1
      else 0
  }

  case class EqualTo(version: Int, typeId: Int, lengthTypeId: Int, content: Vector[Packet]) extends Operator(version, typeId, lengthTypeId, content) {
    override def value: Long =
      if (content(0).value == content(1).value) 1
      else 0
  }

  def version(n: String): Int = Integer.parseInt(n.take(3), 2)

  def typeId(n: String): Int = Integer.parseInt(n.slice(3, 6), 2)

  def lengthTypeId(n: String): Int = Integer.parseInt(n.slice(6, 7), 2)

  def bitLength(n: String): Int = Integer.parseInt(n.slice(7, 22), 2)

  def packetLength(n: String): Int = Integer.parseInt(n.slice(7, 18), 2)

  def parseLiteralPacket(n: String): (Literal, String) = {
    val (content, _, remaining) = n
      .drop(6)
      .sliding(5, 5)
      .foldLeft(("", false, "")) { case ((acc, end, remaining), n) =>
        if (end)
          (acc, end, remaining + n)
        else if (n(0) == '1')
          (acc + n.substring(1), false, "")
        else
          (acc + n.substring(1), true, "")
      }


    (Literal(version(n), typeId(n), java.lang.Long.parseLong(content, 2)), remaining)
  }

  def parseSubPackets(str: String, n: Int): (Vector[Packet], String) = {
    Range(0, n)
      .foldLeft((Vector[Packet](), str)) {
        case ((acc, remaining), _) =>
          val (packet, r) = parsePacket(remaining)
          (acc.appended(packet), r)
      }
  }

  def parseSubPacketsWithFixedWidth(str: String): Vector[Packet] = {
    @tailrec
    def rec(str: String, acc: Vector[Packet]): Vector[Packet] = {
      if (str.isEmpty) acc
      else {
        val (packet, remaining) = parsePacket(str)
        rec(remaining, acc.appended(packet))
      }
    }

    rec(str, Vector())
  }

  def parseOperator(n: String): (Packet, String) = {
    val (subPackets, remaining) = lengthTypeId(n) match {
      case 1 => parseSubPackets(n.substring(18), packetLength(n))
      case 0 => (parseSubPacketsWithFixedWidth(n.substring(22, 22 + bitLength(n))), n.substring(22 + bitLength(n)))
    }

    val operator = typeId(n) match {
      case 0 => Sum(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 1 => Product(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 2 => Minimum(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 3 => Maximum(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 5 => GreaterThan(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 6 => LessThan(version(n), typeId(n), lengthTypeId(n), subPackets)
      case 7 => EqualTo(version(n), typeId(n), lengthTypeId(n), subPackets)
    }

    (operator, remaining)
  }

  def parsePacket(n: String): (Packet, String) = typeId(n) match {
    case 4 => parseLiteralPacket(n)
    case _ => parseOperator(n)
  }

  def parseInput(input: String): String = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .head
      .map(x => Integer.parseInt(x.toString, 16).toBinaryString)
      .map("%4s".format(_).replace(' ', '0'))
      .mkString
  }

  override def solveFirstPart(input: String): Int = {
    val (packet, _) = parsePacket(parseInput(input))

    packet.totalVersionSum
  }

  override def solveSecondPart(input: String): Long = {
    val (packet, _) = parsePacket(parseInput(input))

    packet.value
  }
}
