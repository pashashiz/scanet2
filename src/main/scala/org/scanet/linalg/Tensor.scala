package org.scanet.linalg

import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}
import org.scanet.core.Generator.uniform
import org.scanet.core.NativeArray._
import org.scanet.core.{Buffer, NativeArray, _}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.{specialized => sp}
import org.scanet.instances.core._

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

  def rank: Int = dims.size

  def isScalar: Boolean = rank == 0

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

  implicit def shapeToTensorShape(shape: Shape): TensorShape =
    new TensorShape(shape.dims.map(_.toLong).toArray)

}

class Tensor[@sp A: Numeric](val shape: Shape, val native: NativeTensor) {

  val buffer: Buffer[A] = {
    def data: NativeArray[Byte] = native.tensor_data()

    data.to[A].asBuffer
  }

  def toScalar: A = buffer.get(0)

  def toArray: Array[A] = buffer.toArray

  def toStream: Stream[A] = {
    def next(index: Int): Stream[A] = {
      if (buffer.limit == index) Stream.empty
      else buffer.get(index) #:: next(index + 1)
    }

    next(buffer.position)
  }

  // indexing and slicing
  // todo

  override def toString: String = s"Tensor[${Numeric[A].show}](shape=$shape, size=${buffer.limit}): ${show()}"

  def show(size: Int = 20): String = {
    if (shape.isScalar) {
      buffer.get(buffer.position).toString
    } else {
      // todo: format and limit when slicing is implemented
      // vector: [1, 2, 3]
      // matrix:
      // [
      //   [1, 2, 3],
      //   [4, 5, 6]
      // ]
      // n3:
      // [
      //   [
      //     [1, 2, 3],
      //     [4, 5, 6]
      //   ]
      // ]
      s"[${toStream.take(size).mkString(", ")}]"
    }
  }

  override def hashCode(): Int = shape.hashCode() + buffer.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case other: Tensor[A] => other.shape == shape && other.buffer == buffer
    case _ => false
  }
}

object Tensor extends NumericInstances {

  def allocate[@sp A: Numeric](shape: Shape): Tensor[A] = {
    Tensor(new NativeTensor(Numeric[A].tag, shape))
  }

  def apply[@sp A: Numeric](native: NativeTensor): Tensor[A] =
  // note: pre-initialized shape to overcome @sp issue https://github.com/scala/bug/issues/4511
    new Tensor(Shape.of(native), native)

  def apply[@sp A: Numeric](data: Buffer[A], shape: Shape): Tensor[A] = {
    val tensor = allocate[A](shape)
    tensor.buffer.put(data)
    tensor.buffer.rewind
    tensor
  }

  def apply[@sp A: Numeric](data: Array[A], shape: Shape): Tensor[A] = {
    require(data.length == shape.power,
      s"Shape$shape requires ${shape.power} elements but was passed ${data.length}")
    apply(Buffer.wrap(data), shape)
  }

  def scalar[@sp A: Numeric](value: A): Tensor[A] = apply(Array(value)(Numeric[A].classTag), Shape())

  def vector(range: Range): Tensor[Int] = apply[Int](range.toArray[Int], Shape(range.length))

  def vector[@sp A: Numeric](array: Array[A]): Tensor[A] = apply(array, Shape(array.length))

  def vector[@sp A: Numeric](elements: A*): Tensor[A] = vector(elements.toArray(Numeric[A].classTag))

  def matrix[@sp A: Numeric](rows: Array[A]*): Tensor[A] = {
    require(rows.nonEmpty, "at least one row is required")
    val rowSizes = rows.toList.map(_.length)
    require(rowSizes.distinct.size == 1, "all rows should have the same length")
    val data = rows.foldLeft(new ArrayBuffer[A](rowSizes.sum))((buffer, row) => buffer ++= row).toArray(Numeric[A].classTag)
    apply(data, Shape(rowSizes.length, rowSizes.head))
  }

  def zeros[@sp A: Numeric](shape: Int*): Tensor[A] =
    zeros(Shape(shape.toList))

  def zeros[@sp A: Numeric](shape: Shape): Tensor[A] =
    Tensor(Buffer.allocate[A](shape.power), shape)

  def ones[@sp A: Numeric](shape: Int*): Tensor[A] =
    ones(Shape(shape.toList))

  def ones[@sp A: Numeric](shape: Shape): Tensor[A] =
    fill(shape)(Numeric[A].one)

  def fill[@sp A: Numeric](shape: Int*)(value: A): Tensor[A] =
    fill(Shape(shape.toList))(value)

  def fill[@sp A: Numeric](shape: Shape)(value: A): Tensor[A] =
    Tensor(Buffer.tabulate[A](shape.power)(_ => value), shape)

  def tabulate[@sp A: Numeric](d1: Int)(f: Int => A): Tensor[A] =
    tabulate(Shape(d1))(idx => f(idx.head))

  def tabulate[@sp A: Numeric](d1: Int, d2: Int)(f: (Int, Int) => A): Tensor[A] =
    tabulate(Shape(d1, d2))(idx => f(idx.head, idx(1)))

  def tabulate[@sp A: Numeric](d1: Int, d2: Int, d3: Int)(f: (Int, Int, Int) => A): Tensor[A] =
    tabulate(Shape(d1, d2, d3))(idx => f(idx.head, idx(1), idx(2)))

  def tabulate[@sp A: Numeric](shape: Shape)(f: List[Int] => A): Tensor[A] = {
    // note: could be optimized, cause indexOf is a reverse operation
    val buffer = Buffer.tabulate[A](shape.power)(index => f(shape.indexOf(index)))
    Tensor(buffer, shape)
  }

  def diag[@sp A: Numeric](values: A*): Tensor[A] =
    diag(values.toArray(Numeric[A].classTag))

  def diag[@sp A: Numeric](values: Array[A]): Tensor[A] = {
    val zero = Numeric[A].zero
    tabulate(values.length, values.length)((x, y) =>
      if (x == y) values(x) else zero)
  }

  def eye[@sp A: Numeric](n: Int): Tensor[A] =
    diag[A](Array.fill(n)(Numeric[A].one)(Numeric[A].classTag))

  def linspace[@sp A: Numeric](first: A, last: A, size: Int = 100): Tensor[A] = {
    val increment = (last - first) / (size - 1)
    tabulate(size)(i => first plus (increment * i))
  }

  def range[@sp A: Numeric](start: A, end: A, step: A, inclusive: Boolean = false): Tensor[A] = {
    val sizeAprox = ((end - start) / step).toInt + 1
    val endAprox = start.plus(step * (sizeAprox - 1))
    val size =
      if (endAprox < end || inclusive && (endAprox === end)) {
        sizeAprox
      } else {
        sizeAprox - 1
      }
    tabulate(size.toInt)(i => start plus (step * i))
  }

  def rand[@sp A: Numeric: Dist](shape: Shape, gen: Generator = uniform): Tensor[A] = {
    val (_, arr) = Random[A](gen).next(shape.power)(Numeric[A].classTag, Dist[A])
    Tensor[A](arr, shape)
  }
}
