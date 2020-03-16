package org.scanet.core

import org.bytedeco.javacpp.{BytePointer, DoublePointer, FloatPointer, IntPointer, LongPointer, Pointer, ShortPointer}
import org.scanet.core.TypeTags.{ByteTag, DoubleTag, FloatTag, IntTag, LongTag, ShortTag}

import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions

class NativeArray[A: ClassTag](val pointer: Pointer) {

  def asFloat: FloatPointer = pointer.asInstanceOf[FloatPointer]
  def asDouble: DoublePointer = pointer.asInstanceOf[DoublePointer]
  def asLong: LongPointer = pointer.asInstanceOf[LongPointer]
  def asInt: IntPointer = pointer.asInstanceOf[IntPointer]
  def asShort: ShortPointer = pointer.asInstanceOf[ShortPointer]
  def asByte: BytePointer = pointer.asInstanceOf[BytePointer]

  def to[B: ClassTag]: NativeArray[B] = {
    val currentClass = classTag[A]
    val newClass = classTag[B]
      if (currentClass == newClass) {
        this.asInstanceOf[NativeArray[B]]
      }
      else {
        val newPointer = newClass match {
          case FloatTag => new FloatPointer(pointer)
          case DoubleTag => new DoublePointer(pointer)
          case LongTag => new LongPointer(pointer)
          case IntTag => new IntPointer(pointer)
          case ShortTag => new ShortPointer(pointer)
          case ByteTag => new BytePointer(pointer)
          case _ => error(s"Pointer[${classTag[A]}] is not supported")
        }
        val sizeInBytes = pointer.capacity() * pointer.sizeof()
        val size: Long = sizeInBytes / newPointer.sizeof()
        val newPointer1: Pointer = newPointer.position(0L)
        val newPointer2: Pointer = newPointer1.capacity(size)
        new NativeArray(newPointer2)
      }
  }

  def asBuffer[B: ClassTag]: Buffer[B] = {
    Buffer[B](to[B].pointer.asBuffer())
  }

  def position: Long = pointer.position()

  def position(position: Long): NativeArray[A] = {pointer.position(position); this;}

  def limit: Long = pointer.limit()

  def limit(limit: Long): NativeArray[A] = {pointer.limit(limit); this;}

  def capacity(capacity: Long): NativeArray[A] = {pointer.capacity(capacity); this;}

  def capacity: Long = pointer.capacity()

  def get: A = get(0)

  def get(i: Int): A = classTag[A] match {
    case FloatTag => asFloat.get(i).asInstanceOf[A]
    case DoubleTag => asDouble.get(i).asInstanceOf[A]
    case LongTag => asLong.get(i).asInstanceOf[A]
    case IntTag => asInt.get(i).asInstanceOf[A]
    case ShortTag => asShort.get(i).asInstanceOf[A]
    case ByteTag => asByte.get(i).asInstanceOf[A]
    case _ => error(s"NativeArray[${classTag[A]}] is not supported")
  }

  def get(array: Array[A]): NativeArray[A] = get(array, 0, array.length)

  def get(array: Array[A], offset: Int, length: Int): NativeArray[A] = classTag[A] match {
    case FloatTag => asFloat.get(array.asInstanceOf[Array[Float]], offset, length); this;
    case DoubleTag => asDouble.get(array.asInstanceOf[Array[Double]], offset, length); this;
    case LongTag => asLong.get(array.asInstanceOf[Array[Long]], offset, length); this;
    case IntTag => asInt.get(array.asInstanceOf[Array[Int]], offset, length); this;
    case ShortTag => asShort.get(array.asInstanceOf[Array[Short]], offset, length); this;
    case ByteTag => asByte.get(array.asInstanceOf[Array[Byte]], offset, length); this;
    case _ => error(s"NativeArray[${classTag[A]}] is not supported")
  }

  def putAll(array: Array[A]): NativeArray[A] = put(array, 0, array.length)

  def put(v: A): NativeArray[A] = put(0, v)

  def put(i: Int, v: A): NativeArray[A] = put(Array(v), i, 1)

  def put(array: Array[A], offset: Int, length: Int): NativeArray[A] = classTag[A] match {
    case FloatTag => asFloat.put(array.asInstanceOf[Array[Float]], offset, length); this;
    case DoubleTag => asDouble.put(array.asInstanceOf[Array[Double]], offset, length); this;
    case LongTag => asLong.put(array.asInstanceOf[Array[Long]], offset, length); this;
    case IntTag => asInt.put(array.asInstanceOf[Array[Int]], offset, length); this;
    case ShortTag => asShort.put(array.asInstanceOf[Array[Short]], offset, length); this;
    case ByteTag => asByte.put(array.asInstanceOf[Array[Byte]], offset, length); this;
    case _ => error(s"NativeArray[${classTag[A]}] is not supported")
  }

  def toArray: Array[A] = {
    val array = new Array[A](capacity.toInt)
    for (i <- array.indices) {
      array(i) = get(i)
    }
    array
  }

}

object NativeArray {

  loadNative()

  def apply[A: ClassTag](size: Int): NativeArray[A] = classTag[A] match {
    case FloatTag => new NativeArray[A](new FloatPointer(size.toLong))
    case DoubleTag => new NativeArray[A](new DoublePointer(size.toLong))
    case LongTag => new NativeArray[A](new LongPointer(size.toLong))
    case IntTag => new NativeArray[A](new IntPointer(size.toLong))
    case ShortTag => new NativeArray[A](new ShortPointer(size.toLong))
    case ByteTag => new NativeArray[A](new BytePointer(size.toLong))
    case _ => error(s"NativeArray[${classTag[A]}] is not supported")
  }

  def apply[A: ClassTag](array: A*): NativeArray[A] = {
    NativeArray(array.length).putAll(array.toArray)
  }

  def apply[A: ClassTag](array: Array[A]): NativeArray[A] = {
    NativeArray(array.length).putAll(array)
  }

  // ## Implicit conversions ##
  // TPointer -> NativeArray[T]
  // XPointer -> Array[T]
  // NativeArray[T] -> Array[T]
  // Array[T] -> NativeArray[T]
  // Array[T] -> TPointer

  implicit def pointerArrayToArray[A: ClassTag](pointer: NativeArray[A]): Array[A] = pointer.toArray
  implicit def arrayToPointerArray[A: ClassTag](array: Array[A]): NativeArray[A] = NativeArray(array)

  implicit def floatPointerToPointerArray(pointer: FloatPointer): NativeArray[Float] = new NativeArray[Float](pointer)
  implicit def floatPointerToArray(pointer: FloatPointer): Array[Float] = floatPointerToPointerArray(pointer)
  implicit def pointerArrayToFloatPointer(pointer: NativeArray[Float]): FloatPointer = pointer.asFloat
  implicit def arrayToFloatPointer(array: Array[Float]): FloatPointer = array.asFloat

  implicit def doublePointerToPointerArray(pointer: DoublePointer): NativeArray[Double] = new NativeArray[Double](pointer)
  implicit def doublePointerToArray(pointer: DoublePointer): Array[Double] = doublePointerToPointerArray(pointer)
  implicit def pointerArrayToDoublePointer(pointer: NativeArray[Double]): DoublePointer = pointer.asDouble
  implicit def arrayToDoublePointer(array: Array[Double]): DoublePointer = array.asDouble

  implicit def longPointerToPointerArray(pointer: LongPointer): NativeArray[Long] = new NativeArray[Long](pointer)
  implicit def longPointerToArray(pointer: LongPointer): Array[Long] = longPointerToPointerArray(pointer)
  implicit def pointerArrayToLongPointer(pointer: NativeArray[Long]): LongPointer = pointer.asLong
  implicit def arrayToLongPointer(array: Array[Long]): LongPointer = array.asLong

  implicit def intPointerToPointerArray(pointer: IntPointer): NativeArray[Int] = new NativeArray[Int](pointer)
  implicit def intPointerToArray(pointer: IntPointer): Array[Int] = intPointerToPointerArray(pointer)
  implicit def pointerArrayToIntPointer(pointer: NativeArray[Int]): IntPointer = pointer.asInt
  implicit def arrayToIntPointer(array: Array[Int]): IntPointer = array.asInt

  implicit def shortPointerToPointerArray(pointer: ShortPointer): NativeArray[Short] = new NativeArray[Short](pointer)
  implicit def shortPointerToArray(pointer: ShortPointer): Array[Short] = shortPointerToPointerArray(pointer)
  implicit def pointerArrayToShortPointer(pointer: NativeArray[Short]): ShortPointer = pointer.asShort
  implicit def arrayToShortPointer(array: Array[Short]): ShortPointer = array.asShort

  implicit def bytePointerToPointerArray(pointer: BytePointer): NativeArray[Byte] = new NativeArray[Byte](pointer)
  implicit def bytePointerToArray(pointer: BytePointer): Array[Byte] = bytePointerToPointerArray(pointer)
  implicit def pointerArrayToBytePointer(pointer: NativeArray[Byte]): BytePointer = pointer.asByte
  implicit def arrayToBytePointer(array: Array[Byte]): BytePointer = array.asByte
}
