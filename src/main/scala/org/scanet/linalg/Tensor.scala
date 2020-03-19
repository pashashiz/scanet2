package org.scanet.linalg

import org.bytedeco.javacpp.BytePointer
import org.scanet.core.{Buffer, NativeArray, _}
import org.bytedeco.tensorflow.global.tensorflow.{DT_DOUBLE, DT_FLOAT, DT_INT16, DT_INT32, DT_INT64, DT_INT8}
import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}
import org.scanet.core.TypeTags.{ByteTag, DoubleTag, FloatTag, IntTag, LongTag, ShortTag}
import org.scanet.core.NativeArray._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions

case class Shape(dims: List[Int]) {

  val power: Int = dims.product

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

class Tensor[A: ClassTag](val native: NativeTensor) {

  val shape: Shape = Shape.of(native)

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

  override def toString: String = s"Tensor[${classTag[A]}](shape=$shape, size=${buffer.limit})"

  def showData(size: Int = 20): String = {
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
      s"[${toArray.mkString(", ")}]"
    }
  }

  def show(size: Int = 20): String =  s"$toString: ${showData(size)}"

}

object Tensor {

  private def nativeTypeOf[A: ClassTag]: Int = classTag[A] match {
    case FloatTag => DT_FLOAT
    case DoubleTag => DT_DOUBLE
    case LongTag => DT_INT64
    case IntTag => DT_INT32
    case ShortTag => DT_INT16
    case ByteTag => DT_INT8
    case _ => error(s"Type ${classTag[A]} is not supported")
  }

  def allocate[A: ClassTag](shape: Shape): Tensor[A] = {
    Tensor(new NativeTensor(nativeTypeOf[A], shape))
  }

  def apply[A: ClassTag](native: NativeTensor): Tensor[A] = new Tensor(native)

  def apply[A: ClassTag](data: Buffer[A], shape: Shape): Tensor[A] = {
    val tensor = allocate[A](shape)
    tensor.buffer.put(data)
    tensor.buffer.rewind
    tensor
  }

  def apply[A: ClassTag](data: Array[A], shape: Shape): Tensor[A] = {
    require(data.length == shape.power,
      s"Shape$shape requires ${shape.power} elements but was passed ${data.length}")
    apply(Buffer.wrap(data), shape)
  }

  def scalar[A: ClassTag](value: A): Tensor[A] = apply(Array(value), Shape())

  def vector[A: ClassTag](array: Array[A]): Tensor[A] = apply(array, Shape(array.length))

  def vector[A: ClassTag](elements: A*): Tensor[A] = vector(elements.toArray)

  def matrix[A: ClassTag](rows: Array[A]*): Tensor[A] = {
    require(rows.nonEmpty, "at least one row is required")
    val rowSizes = rows.toList.map(_.length)
    require(rowSizes.distinct.size == 1, "all rows should have the same length")
    val data = rows.foldLeft(new ArrayBuffer[A](rowSizes.sum))((buffer, row) => buffer ++= row).toArray
    apply(data, Shape(rowSizes.length, rowSizes.head))
  }

}
