package com.github.netfalo.aoc2021

object Day20 extends Problem[Int, Int] {

  type IEA = Vector[Char]

  case class Image(pixels: Vector[Vector[Char]], width: Int, height: Int, void: Char) {
    override def toString: String = pixels.map(_.mkString).mkString("\n")

    def activePixels: Int = pixels.map(_.count(_ == '1')).sum

    def enhance(iea: IEA, times: Int): Image = {
      Range(0, times)
        .foldLeft(this) { (img, _) => img.enhance(iea) }
    }

    def enhance(iea: IEA): Image = {
      val extendedImage =
        Vector(Vector.fill(width + 2)(void)) ++
          pixels.map(row => Vector(void) ++ row ++ Vector(void)) ++
          Vector(Vector.fill(width + 2)(void))

      val relativeIndexes = Vector(
        (-1, -1), (0, -1), (1, -1),
        (-1, 0), (0, 0), (1, 0),
        (-1, 1), (0, 1), (1, 1)
      )

      val enhancedPixels = extendedImage
        .zipWithIndex
        .map(row => (row._1.zipWithIndex, row._2))
        .map {
          case (row, y) =>
            row.map {
              case (_, x) =>
                val bn = relativeIndexes
                  .map {
                    case (_x, _y) if _x + x < 0 || _y + y < 0 || _y + y >= height + 2 || _x + x >= width + 2 => void
                    case (_x, _y) => extendedImage(y + _y)(x + _x)
                  }
                  .mkString
                val index = Integer.parseInt(bn, 2)

                iea(index)
            }
        }

      val newVoid = Integer.parseInt(Vector.fill(9)(void).mkString, 2)

      Image(enhancedPixels, width + 2, height + 2, iea(newVoid))
    }
  }

  def parseInput(example: String): (IEA, Image) = {
    val lines = example.split('\n').filterNot(_.isBlank)
    val iea: IEA = lines.head
      .map {
        case '.' => '0'
        case '#' => '1'
      }
      .toVector

    val pixels = lines.tail
      .map(_.map {
        case '.' => '0'
        case '#' => '1'
      }.toVector)
      .toVector

    val image = Image(pixels, pixels.head.length, pixels.length, '0')

    assert(iea.length == 512)

    (iea, image)
  }

  override def solveFirstPart(input: String): Int = {
    val (iea, image) = parseInput(input)

    image.enhance(iea, 2).activePixels
  }

  override def solveSecondPart(input: String): Int = {
    val (iea, image) = parseInput(input)

    image.enhance(iea, 50).activePixels
  }

}
