package com.github.netfalo.aoc2021

import com.github.netfalo.aoc2021.Day16._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day16Spec extends AnyFlatSpec with Matchers {

  private lazy val exercise = Resource("Day16.txt").content

  "Day 16" should "parse" in {
    parseInput("D2FE28") shouldEqual "110100101111111000101000"
    parseInput("38006F45291200") shouldEqual "00111000000000000110111101000101001010010001001000000000"
    parseInput("EE00D40C823060") shouldEqual "11101110000000001101010000001100100000100011000001100000"
  }

  it should "get version" in {
    version(parseInput("D2FE28")) shouldEqual 6
    version(parseInput("38006F45291200")) shouldEqual 1
    version(parseInput("EE00D40C823060")) shouldEqual 7
  }

  it should "get typeId" in {
    typeId(parseInput("D2FE28")) shouldEqual 4
    typeId(parseInput("38006F45291200")) shouldEqual 6
    typeId(parseInput("EE00D40C823060")) shouldEqual 3
  }

  it should "get lengthTypeId" in {
    lengthTypeId(parseInput("38006F45291200")) shouldEqual 0
    lengthTypeId(parseInput("EE00D40C823060")) shouldEqual 1
  }

  it should "get bitLength" in {
    bitLength(parseInput("38006F45291200")) shouldEqual 27
  }

  it should "get packetLength" in {
    packetLength(parseInput("EE00D40C823060")) shouldEqual 3
  }

  it should "parse literal packet" in {
    parseLiteralPacket(parseInput("D2FE28")) shouldEqual(Literal(6, 4, 2021), "000")
    parseLiteralPacket("01010000001100100000100011000001100000") shouldEqual(Literal(2, 4, 1), "100100000100011000001100000")
  }

  it should "parse operator packet" in {
    parsePacket(parseInput("38006F45291200")) shouldEqual(LessThan(1, 6, 0, Vector(Literal(6, 4, 10), Literal(2, 4, 20))), "0000000")
    parsePacket(parseInput("EE00D40C823060")) shouldEqual(Maximum(7, 3, 1, Vector(Literal(2, 4, 1), Literal(4, 4, 2), Literal(1, 4, 3))), "00000")
  }

  it should "sum all versions" in {
    parsePacket(parseInput("8A004A801A8002F478"))._1.totalVersionSum shouldEqual 16
    parsePacket(parseInput("620080001611562C8802118E34"))._1.totalVersionSum shouldEqual 12
    parsePacket(parseInput("C0015000016115A2E0802F182340"))._1.totalVersionSum shouldEqual 23
    parsePacket(parseInput("A0016C880162017C3686B18A3D4780"))._1.totalVersionSum shouldEqual 31
  }

  it should "sum all versions in exercise" in {
    val (packet, remainder) = parsePacket(parseInput(exercise))
    packet.totalVersionSum shouldEqual 917
  }

  it should "calculate value" in {
    parsePacket(parseInput("C200B40A82"))._1.value shouldEqual 3
    parsePacket(parseInput("04005AC33890"))._1.value shouldEqual 54
    parsePacket(parseInput("880086C3E88112"))._1.value shouldEqual 7
    parsePacket(parseInput("CE00C43D881120"))._1.value shouldEqual 9
    parsePacket(parseInput("D8005AC2A8F0"))._1.value shouldEqual 1
    parsePacket(parseInput("F600BC2D8F"))._1.value shouldEqual 0
    parsePacket(parseInput("9C005AC2F8F0"))._1.value shouldEqual 0
    parsePacket(parseInput("9C0141080250320F1802104A08"))._1.value shouldEqual 1
  }

  it should "calculate value for exercise" in {
    parsePacket(parseInput(exercise))._1.value shouldEqual 0
  }
}
