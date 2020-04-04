package org.scanet.linalg

import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}

import scala.annotation.tailrec
import org.scanet.core.NativeArray._
import org.scanet.linalg.Slice.syntax._

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

  def isInBound(index: Index): Boolean = {
    val rankInRange = index.rank <= rank
    val numOutOfBounds = index.indexes.zip(dims)
      .map { case (index: Int, max: Int) => index + 1 - max }
      .count(_ > 0)
    rankInRange && numOutOfBounds == 0
  }

  def isInBound(projection: Projection): Boolean = {
    val rankInRange = projection.rank <= rank
    val numOutOfBounds = projection.slices.zip(dims)
      .map { case (slice: Slice, max: Int) => slice.until - max }
      .count(_ > 0)
    rankInRange && numOutOfBounds == 0
  }

  def head: Int = dims.head

  def tail: Shape = Shape(dims.tail)

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

case class Projection(slices: List[Slice]) {
  def head: Slice = slices.head
  def tail: Projection = Projection(slices.tail)
  def isEmpty: Boolean = slices.isEmpty
  def rank: Int = slices.size
  def adjustTo(shape: Shape): Projection = {
    require(shape.isInBound(this),
      s"projection $this is out of bound, should be within $shape")
    val filledSlices = shape.dims
      .zip(alignRightWith(Projection.of(shape)).slices)
      .map { case (shapeSize: Int, slice: Slice) =>
        if (slice.isOpenedRight) (slice.from until shapeSize).build else slice}
    Projection(filledSlices)
  }

  def shape: Shape = Shape(slices.map(_.size).dropWhile(_ == 1))

  def alignLeftWith(other: Projection): Projection = alignWith(other, leftSide = true)
  def alignRightWith(other: Projection): Projection = alignWith(other, leftSide = false)
  def alignWith(other: Projection, leftSide: Boolean): Projection = {
    if (rank < other.rank) {
      val dimsToFill = other.rank - rank
      val filledSlices = if (dimsToFill > 0) {
        if (leftSide) {
          other.slices.take(dimsToFill) ++ slices
        } else {
          slices ++ other.slices.take(dimsToFill)
        }
      } else {
        slices
      }
      Projection(filledSlices)
    } else {
      this
    }
  }

  // (*, *, *) :> (1, 2-4, *) = (1, 2-4, *)
  // (1, 2-4, *) :> (1, 2-4) = (1, 1, 2-4)
  def narrow(other: Projection): Projection = {
    val aligned = other.alignLeftWith(Projection.fill(rank, 0))
    val narrowedSlices = slices.zip(aligned.slices)
      .map { case (sliceThis: Slice, sliceOther: Slice) => sliceThis narrow sliceOther}
    Projection(narrowedSlices)
  }
  override def toString: String = s"(${slices.mkString(", ")})"
}

object Projection {

  def apply[A: CanBuildSliceFrom](a: A): Projection =
    Projection(List(a.build))
  def apply[A: CanBuildSliceFrom, B: CanBuildSliceFrom](a: A, b: B): Projection =
    Projection(List(a.build, b.build))
  def apply[A: CanBuildSliceFrom, B: CanBuildSliceFrom, C: CanBuildSliceFrom](a: A, b: B, c: C): Projection =
    Projection(List(a.build, b.build, c.build))
  def apply[A: CanBuildSliceFrom, B: CanBuildSliceFrom, C: CanBuildSliceFrom, D: CanBuildSliceFrom](a: A, b: B, c: C, d: D): Projection =
    Projection(List(a.build, b.build, c.build, d.build))

  def apply(slices: Seq[Slice]): Projection = new Projection(slices.toList)

  def of(shape: Shape): Projection =
    Projection(shape.dims.map(dim => (0 until dim).build))

  def fill(rank: Int, value: Int): Projection =
    Projection((0 until rank).map(_ => value.build))
}

case class View(shape: Shape, projection: Projection) {

  val projectedShape: Shape = projection.shape

  def narrow(other: Projection): View = new View(shape, projection narrow other)

  def positionOf(viewIndex: Index): Int = narrow(viewIndex.toProjection).positions.head

  // todo: slice -> type class
  // todo: make eager optimized version
  // todo: make fully lazy
  def positions: Stream[Int] = {
    def nestedIndexes(baseIndex: Int, dimsPower: List[Int], projection: Projection): Stream[Int] = {
      if (projection.isEmpty) {
        baseIndex #:: Stream.empty
      } else {
        projection.head.elements
          .map(i => {
            val nextIndex = baseIndex + dimsPower.head * i
            nestedIndexes(nextIndex, dimsPower.tail, projection.tail)
          })
          .foldLeft(Stream.empty[Int])((l, r) => l #::: r)
      }
    }
    nestedIndexes(0, shape.dimsPower, projection)
  }
}

object View {
  def apply(shape: Shape): View = {
    new View(shape, Projection.of(shape))
  }
  def apply(shape: Shape, projection: Projection): View =
    new View(shape, projection.adjustTo(shape))
}

case class Index(indexes: List[Int]) {
  def get(dim: Int): Int = indexes(dim)
  def rank: Int = indexes.size
  def head: Int = indexes.head
  def tail: Index = Index(indexes.tail)
  def toProjection: Projection = Projection(indexes.map(i => i.build))
  override def toString: String = s"(${indexes.mkString(", ")})"
}

object Index {
  def apply(indexes: Int*): Index = Index(indexes.toList)
}

trait IndexConversions {
  implicit def seqToIndex(indexes: List[Int]): Index = Index(indexes)
}