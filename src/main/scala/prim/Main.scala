package prim

import java.io.File

import scala.io.Source

object Main {
  val sourcePath: String = Seq("src", "test", "sources", "").mkString(File.separator)

  def main(args: Array[String]): Unit = {
    val res = new Algo(readMatrix("task")).apply
    println("result:")
    println(res.map({ case (x, y) => (x + 1, y + 1) }).sorted.mkString("\n"))
  }

  def readMatrix(filename: String): Seq[Seq[Int]] =
    Source.fromFile(sourcePath + filename)
      .getLines()
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(
        _.split(" ")
        .map(_.toInt)
        .toSeq
      )
      .toSeq

  def printMatrix(matrix: Seq[Seq[Int]]): Unit = {
    println(matrix.map(_.mkString("\t")).mkString("\n"))
  }
}
