package org.scanet.linalg

import org.scanet.linalg.Slice.syntax._

case class View(originalShape: Shape, projection: Projection) {

  val projectedShapeFull: Shape = projection.shapeFull
  val projectedShapeShort: Shape = projection.shapeShort

  def narrow(other: Projection): View = {
    require(other.rank <= projectedShapeShort.rank,
      s"$other projection's rank ${other.rank} should be less or equal " +
        s"to shape's rank ${projectedShapeShort.rank}")
    val adjusted = other
      .alignRight(projectedShapeShort.rank, ::.build)
      .adjustTo(projectedShapeShort)
      .alignLeft(originalShape.rank, 0.build)
    require(projectedShapeFull.isInBound(adjusted),
      s"$other projection is out of bound, should fit shape $projectedShapeShort")
    new View(originalShape, projection narrow adjusted)
  }

  def positions: Array[Int] = {
    def loop(dims: Seq[(Slice, Int)], absPos: Int, acc: Array[Int], seqPos: Int): (Array[Int], Int) = {
      if (dims.isEmpty) {
        acc(seqPos) = absPos
        (acc, seqPos + 1)
      } else {
        val (slice, dimPower) = dims.head
        slice.elements.foldLeft((acc, seqPos))((a, i) => {
          val nextIndex = absPos + dimPower * i
          loop(dims.tail, nextIndex, a._1, a._2)
        })
      }
    }
    val dims = projection.slices.zip(originalShape.dimsPower)
    val (array, _) = loop(dims, 0, Array.ofDim(projectedShapeShort.power), 0)
    array
  }

  override def toString: String = s"$originalShape x $projection -> $projectedShapeShort"
}

object View {
  def apply(shape: Shape): View = {
    new View(shape, Projection.of(shape))
  }
  def apply(shape: Shape, projection: Projection): View =
    new View(shape, projection.adjustTo(shape))
}