package org.scanet.linalg

import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}
import org.scanet.core.NativeArray._
import org.scanet.core.{Buffer, NativeArray, _}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.classTag
import scala.{specialized => sp}

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

class Tensor[@sp A: Numerical](val shape: Shape, val native: NativeTensor) {

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

  override def toString: String = s"Tensor[${Numerical[A].show}](shape=$shape, size=${buffer.limit}): ${show()}"

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

object Tensor {

  def allocate[@sp A: Numerical](shape: Shape): Tensor[A] = {
    Tensor(new NativeTensor(Numerical[A].tag, shape))
  }

  def apply[@sp A: Numerical](native: NativeTensor): Tensor[A] =
  // note: pre-initialized shape to overcome @sp issue https://github.com/scala/bug/issues/4511
    new Tensor(Shape.of(native), native)

  def apply[@sp A: Numerical](data: Buffer[A], shape: Shape): Tensor[A] = {
    val tensor = allocate[A](shape)
    tensor.buffer.put(data)
    tensor.buffer.rewind
    tensor
  }

  def apply[@sp A: Numerical](data: Array[A], shape: Shape): Tensor[A] = {
    require(data.length == shape.power,
      s"Shape$shape requires ${shape.power} elements but was passed ${data.length}")
    apply(Buffer.wrap(data), shape)
  }

  def scalar[@sp A: Numerical](value: A): Tensor[A] = apply(Array(value)(Numerical[A].classTag), Shape())

  def vector[@sp A: Numerical](array: Array[A]): Tensor[A] = apply(array, Shape(array.length))

  def vector[@sp A: Numerical](elements: A*): Tensor[A] = vector(elements.toArray(Numerical[A].classTag))

  def matrix[@sp A: Numerical](rows: Array[A]*): Tensor[A] = {
    require(rows.nonEmpty, "at least one row is required")
    val rowSizes = rows.toList.map(_.length)
    require(rowSizes.distinct.size == 1, "all rows should have the same length")
    val data = rows.foldLeft(new ArrayBuffer[A](rowSizes.sum))((buffer, row) => buffer ++= row).toArray(Numerical[A].classTag)
    apply(data, Shape(rowSizes.length, rowSizes.head))
  }

  def zeros[@sp A: Numerical](shape: Int*): Tensor[A] =
    zeros(Shape(shape.toList))

  def zeros[@sp A: Numerical](shape: Shape): Tensor[A] =
    Tensor(Buffer.allocate[A](shape.power), shape)

  def fill[@sp A: Numerical](shape: Int*)(value: A): Tensor[A] =
    fill(Shape(shape.toList))(value)

  def fill[@sp A: Numerical](shape: Shape)(value: A): Tensor[A] =
    Tensor(Buffer.tabulate[A](shape.power)(_ => value), shape)

  def tabulate[@sp A: Numerical](d1: Int)(f: Int => A): Tensor[A] =
    tabulate(Shape(d1))(idx => f(idx.head))

  def tabulate[@sp A: Numerical](d1: Int, d2: Int)(f: (Int, Int) => A): Tensor[A] =
    tabulate(Shape(d1, d2))(idx => f(idx.head, idx(1)))

  def tabulate[@sp A: Numerical](d1: Int, d2: Int, d3: Int)(f: (Int, Int, Int) => A): Tensor[A] =
    tabulate(Shape(d1, d2, d3))(idx => f(idx.head, idx(1), idx(2)))

  def tabulate[@sp A: Numerical](shape: Shape)(f: List[Int] => A): Tensor[A] = {
    // note: could be optimized, cause indexOf is a reverse operation
    val buffer = Buffer.tabulate[A](shape.power)(index => f(shape.indexOf(index)))
    Tensor(buffer, shape)
  }

  def diag[@sp A: Numerical](values: A*): Tensor[A] = diag(values.toArray(Numerical[A].classTag))

  def diag[@sp A: Numerical](values: Array[A]): Tensor[A] = ???

  // todo: need to add Numeric[A] typeclass, needs zero
  def eye[@sp A: Numerical](n: Int): Tensor[A] = ???

  // todo: need to add Numeric[A] typeclass, will work for int, long, short, byte only
  def range[@sp A: Numerical](from: A, to: A): Tensor[A] = ???

  def linspace[@sp A: Numerical](first: A, second: A, elements: Int): Tensor[A] = ???


  // todo: more methods to create tensors

}
