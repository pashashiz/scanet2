package org.scanet.core

import org.bytedeco.javacpp.Pointer
import org.bytedeco.tensorflow.LongVector
import org.scanet.core.TypeTags.LongTag

import scala.reflect.{ClassTag, classTag}

object TypeTags {
  val FloatTag: ClassTag[Float] = classTag[Float]
  val DoubleTag: ClassTag[Double] = classTag[Double]
  val LongTag: ClassTag[Long] = classTag[Long]
  val IntTag: ClassTag[Int] = classTag[Int]
  val ShortTag: ClassTag[Short] = classTag[Short]
  val ByteTag: ClassTag[Byte] = classTag[Byte]
}

class NativeVector[A: ClassTag](val pointer: Pointer) {

  def asLong: LongVector = pointer.asInstanceOf[LongVector]

  def empty: Boolean = size == 0

  def size: Long = classTag[A] match {
    case LongTag => asLong.size()
    case _ => error(s"NativeVector[${classTag[A]}] is not supported")
  }

  def get(i: Long): A = classTag[A] match {
    case LongTag => asLong.get(i).asInstanceOf[A]
    case _ => error(s"NativeVector[${classTag[A]}] is not supported")
  }

  def put(i: Long, value: A): NativeVector[A] = classTag[A] match {
    case _ => error(s"NativeVector[${classTag[A]}] is not supported")
  }

  def putAll(other: NativeVector[A]): NativeVector[A] = classTag[A] match {
    case _ => error(s"NativeVector[${classTag[A]}] is not supported")
  }

  def toArray: Array[A] = {
    val array = new Array[A](size.toInt)
    for (i <- array.indices) {
      array(i) = get(i)
    }
    array
  }

  override def toString: String = s"NativeVector[${classTag[A]}]($size)"

  def show: String = s"[${toArray.mkString(", ")}]"

  override def hashCode(): Int = toArray.hashCode()

  override def equals(that: Any): Boolean = that match {
    case that: NativeVector[A] => this.toArray sameElements that.toArray
    case _ => false
  }
}

object NativeVector {

  loadNative()

  def apply[A: ClassTag](pointer: Pointer): NativeVector[A] = new NativeVector[A](pointer)

  def apply[A: ClassTag](): NativeVector[A] = classTag[A] match {
    case _ => error(s"NativeVector[${classTag[A]}] is not supported")
  }

  def apply[A: ClassTag](array: Array[A]): NativeVector[A] = {
    val vector = NativeVector[A]()
    for (i <- array.indices) {
      vector.put(i, array(i))
    }
    vector
  }

  def apply[A: ClassTag](elements: A*): NativeVector[A] = NativeVector[A](elements.toArray)

  implicit def vectorToArray[A: ClassTag](vector: NativeVector[A]): Array[A] = vector.toArray
  implicit def arrayToVector[A: ClassTag](array: Array[A]): NativeVector[A] = NativeVector[A](array)

  implicit def longVectorToVector(vector: LongVector): NativeVector[Long] = NativeVector(vector)
  implicit def vectorToLongVector(vector: NativeVector[Long]): LongVector = vector.asLong
  implicit def arrayToLongVector(array: Array[Long]): LongVector = NativeVector(array)
  implicit def longVectorToArray(vector: LongVector): Array[Long] = NativeVector[Long](vector)

}
