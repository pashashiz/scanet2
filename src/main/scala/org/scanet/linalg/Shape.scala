package org.scanet.linalg

import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}

import scala.annotation.tailrec
import org.scanet.core.NativeArray._

case class Shape(dims: List[Int]) {

  require(dims.forall(_ > 0), "dimension size cannot be 0")
  val power: Int = dims.product
  val dimsPower: List[Int] = {
    val powers = dims.foldLeft(List(power))((power, dim) => power.head / dim :: power)
    powers.reverse.tail
  }

  def indexOf(absPosition: Int): List[Int] = {
    val (indexes, _) = dimsPower.foldLeft((List[Int](), absPosition))(
      (acc, dimPower) => {
        val (indexes, prevPos) = acc
        val index = prevPos / dimPower
        val nextPos = prevPos % dimPower
        (index :: indexes, nextPos)
      })
    indexes.reverse
  }

  def get(dim: Int): Int = dims(dim)

  def rank: Int = dims.size

  def isScalar: Boolean = rank == 0

  def isInBound(projection: Projection): Boolean = {
    val rankInRange = projection.rank <= rank
    val numOutOfBounds = projection.slices.zip(dims)
      .map { case (slice: Slice, max: Int) =>
        if (slice.isOpenedRight) 0 else slice.until - max
      }
      .count(_ > 0)
    rankInRange && numOutOfBounds == 0
  }

  def head: Int = dims.head

  def tail: Shape = Shape(dims.tail)

  def startDimsOf(dim: Int): Int = dims.takeWhile(_ == dim).size

  def alignLeft(size: Int, using: Int): Shape = align(size, using, left = true)
  def alignRight(size: Int, using: Int): Shape = align(size, using, left = false)
  def align(size: Int, using: Int, left: Boolean): Shape = {
    if (rank < size) {
      val dimsToFill = size - rank
      val filledDims = if (dimsToFill > 0) {
        if (left) {
          (0 until dimsToFill).map(_ => using) ++ dims
        } else {
          dims ++ (0 until dimsToFill).map(_ => using)
        }
      } else {
        dims
      }
      Shape(filledDims.toList)
    } else {
      this
    }
  }

  override def toString: String = s"(${dims.mkString(", ")})"
}

object Shape {

  def apply(dims: Int*): Shape = Shape(dims.toList)

  def of(native: NativeTensor): Shape = {
    val numDims = native.dims()
    @tailrec
    def iter(next: Int, acc: List[Int]): List[Int] = {
      if (numDims == next) acc
      else iter(next + 1, native.dim_size(next).toInt :: acc)
    }
    Shape(iter(0, Nil).reverse)
  }

  implicit def shapeToTensorShape(shape: Shape): TensorShape = {
    new org.bytedeco.tensorflow.TensorShape(shape.dims.map(_.toLong).toArray)
  }

}







