package prim

import Math.max

import prim.util.Mark

import scala.collection.mutable

class Algo(inputMatrix: Seq[Seq[Int]]) {
  val matrix: Seq[Seq[Int]] =
    inputMatrix
      .zip(inputMatrix.transpose)
      .map({
        case (x, y) =>
          x
            .zip(y)
            .map({ case (a, b) => max(a, b) })
      })
  var selected: Seq[Int] = Seq(0)
  var selectedEdges: Seq[(Int, Int)] = Seq()

  def unselected(): Seq[Int] = matrix.indices.filter(!selected.contains(_))

  var marks: mutable.Map[Int, Mark] = unselected().map(_ -> Mark(-1, Int.MaxValue)).toMap.map(identity)(scala.collection.breakOut)

  def closestSelected(i: Int): (Int, Int) = incident(i).filter(x => selected.contains(x._2)).minBy(_._1)

  def incident(i: Int): Seq[(Int, Int)] = matrix(i).zipWithIndex.filter(_._1 > 0)

  def updateIncident(i: Int): Unit =
    incident(i)
      .filter(x => unselected().contains(x._2))
      .foreach({ case (weight, j) =>
        if (marks(j).weight > weight)
          marks(j) = Mark(i, weight)
      })


  def apply: Seq[(Int, Int)] = {
    Main.printMatrix(matrix)
//    println()

    while (unselected().nonEmpty) {
//      println(s"\nselected: $selected ")
      val (u, s, _) = unselected()
        .map(x => {
          val c =
            try {
              closestSelected(x)
            } catch {
              case _: UnsupportedOperationException => (Int.MaxValue, -1)
            }
//          println(s"${c._2}, ${incident(x)}")
          (x, c._2, c._1) // unselected, closest selected, weight
        })
        .minBy(_._3)
      selected ++= Seq(u)
      selectedEdges ++= Seq((s, u))
      updateIncident(u)
    }
    selectedEdges
  }

}
