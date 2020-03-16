package org.scanet.linalg

import org.bytedeco.javacpp.BytePointer
import org.scanet.core.{Buffer, NativeArray, _}
import org.bytedeco.tensorflow.global.tensorflow.{DT_DOUBLE, DT_FLOAT, DT_INT16, DT_INT32, DT_INT64, DT_INT8}
import org.bytedeco.tensorflow.{TensorShape, Tensor => NativeTensor}
import org.scanet.core.TypeTags.{ByteTag, DoubleTag, FloatTag, IntTag, LongTag, ShortTag}
import org.scanet.core.NativeArray._

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

class Tensor[A: ClassTag](val native: NativeTensor) {

  val shape: List[Long] = {
    val numDims = native.dims()
    @tailrec
    def iter(next: Int, acc: List[Long]): List[Long] = {
      if (numDims == next) acc
      else iter(next + 1, native.dim_size(next) :: acc)
    }
    iter(0, Nil)
  }

  val buffer: Buffer[A] = {
    def data: NativeArray[Byte] = native.tensor_data()
    data.asBuffer[A]
  }

  def toScalar: A = buffer.get(0)

  def toArray: Array[A] = buffer.array

  override def toString = s"Tensor[${classTag[A]}](shape=(${shape.mkString(", ")}), size=${buffer.limit})"

  // todo: format and limit
  def show: String = toString + ":\n" + s"[${toArray.mkString(", ")}]"

  def check: Unit = {
    println(native.tensor_data())
  }

  def data: BytePointer = native.tensor_data()

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

  def allocate[A: ClassTag](shape: List[Long]): Tensor[A] = {
    Tensor(new NativeTensor(nativeTypeOf[A], new TensorShape(shape.toArray)))
  }

  def apply[A: ClassTag](native: NativeTensor): Tensor[A] = new Tensor(native)

  def apply[A: ClassTag](data: Buffer[A], shape: List[Long]): Tensor[A] = {
    val tensor = allocate[A](shape)
    tensor.buffer.put(data)
    tensor.buffer.position(0)
    tensor
  }

  def apply[A: ClassTag](data: Array[A], shape: List[Long]): Tensor[A] =
    apply(Buffer.wrap(data), shape)

  def scalar[A: ClassTag](value: A): Tensor[A] = apply(Array(value), Nil)

  def vector[A: ClassTag](array: Array[A]): Tensor[A] = apply(array, List(array.length.toLong))

  def vector[A: ClassTag](elements: A*): Tensor[A] = vector(elements.toArray)

}
