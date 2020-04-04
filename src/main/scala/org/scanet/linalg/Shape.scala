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

case class Projection(slices: List[Slice]) {
  def head: Slice = slices.head
  def tail: Projection = Projection(slices.tail)
  def isEmpty: Boolean = slices.isEmpty
  def rank: Int = slices.size
  def adjustTo(shape: Shape): Projection = {
    require(shape.isInBound(this),
      s"$this projection is out of bound, should fit shape $shape")
    val filledSlices = shape.dims
      .zip(alignRight(shape.rank, ::.build).slices)
      .map { case (shapeSize: Int, slice: Slice) => {
        if (slice.isOpenedRight) (slice.from until shapeSize).build else slice}
      }
    Projection(filledSlices)
  }

  def shapeFull: Shape = Shape(slices.map(_.size))
  def shapeShort: Shape = Shape(slices.map(_.size).dropWhile(_ == 1))

  def alignLeft(size: Int, using: Slice): Projection = align(size, using, left = true)
  def alignRight(size: Int, using: Slice): Projection = align(size, using, left = false)
  def align(size: Int, using: Slice, left: Boolean): Projection = {
    if (rank < size) {
      val dimsToFill = size - rank
      val filledSlices = if (dimsToFill > 0) {
        if (left) {
          (0 until dimsToFill).map(_ => using) ++ slices
        } else {
          slices ++ (0 until dimsToFill).map(_ => using)
        }
      } else {
        slices
      }
      Projection(filledSlices.toList)
    } else {
      this
    }
  }

  // (*, *, *) :> (1, 2-4, *) = (1, 2-4, *)
  // (1, 2-4, *) :> (1, 2-4) = (1, 1, 2-4)
  def narrow(other: Projection): Projection = {
    require(other.rank == rank,
      s"given projection's rank ${other.rank} does not match to $rank rank")
    val narrowedSlices = slices.zip(other.slices)
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

  def positionOf(viewIndex: Index): Int = narrow(viewIndex.toProjection).positions.head

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
    nestedIndexes(0, originalShape.dimsPower, projection)
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