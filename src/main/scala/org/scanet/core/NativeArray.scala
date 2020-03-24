package org.scanet.core

import org.bytedeco.javacpp.{BytePointer, DoublePointer, FloatPointer, IntPointer, LongPointer, Pointer, ShortPointer}
import org.scanet.core.Numerical._
import scala.language.implicitConversions
import scala.{specialized => sp}

class NativeArray[@sp A: Numerical](val pointer: Pointer) extends AutoCloseable {

  def asFloat: FloatPointer = pointer.asInstanceOf[FloatPointer]
  def asDouble: DoublePointer = pointer.asInstanceOf[DoublePointer]
  def asLong: LongPointer = pointer.asInstanceOf[LongPointer]
  def asInt: IntPointer = pointer.asInstanceOf[IntPointer]
  def asShort: ShortPointer = pointer.asInstanceOf[ShortPointer]
  def asByte: BytePointer = pointer.asInstanceOf[BytePointer]

  def to[@sp B: Numerical]: NativeArray[B] = {
    val currentType = Numerical[A].tag
    val newType = Numerical[B].tag
      if (currentType == newType) {
        this.asInstanceOf[NativeArray[B]]
      }
      else {
        val newPointer = newType match {
          case FloatTag => new FloatPointer(pointer)
          case DoubleTag => new DoublePointer(pointer)
          case LongTag => new LongPointer(pointer)
          case IntTag => new IntPointer(pointer)
          case ShortTag => new ShortPointer(pointer)
          case ByteTag => new BytePointer(pointer)
          case _ => error(s"Pointer[${Numerical[A].show}] is not supported")
        }
        val sizeInBytes = pointer.capacity() * pointer.sizeof()
        val size: Long = sizeInBytes / newPointer.sizeof()
        val newPointer1: Pointer = newPointer.position(0L)
        val newPointer2: Pointer = newPointer1.capacity(size)
        new NativeArray(newPointer2)
      }
  }

  def asBuffer: Buffer[A] = Buffer[A](pointer.asBuffer())

  def position: Int = pointer.position().toInt

  def position(position: Int): NativeArray[A] = {
    require(position >= 0 && position <= limit, s"position $position should be in a range [0, $limit]")
    pointer.position(position)
    this
  }

  def limit: Int = pointer.limit().toInt

  def limit(limit: Int): NativeArray[A] = {
    require(limit >= position && limit <= capacity, s"limit $limit should be in a range [$position, $capacity)")
    pointer.limit(limit)
    this
  }

  def capacity: Int = pointer.capacity().toInt

  def size: Int = limit - position

  def get: A = get(0)

  def get(i: Int): A = {
    require(i < size, s"index $i is out of bound: [0, $size]")
    Numerical[A].tag match {
      case FloatTag => asFloat.get(i).asInstanceOf[A]
      case DoubleTag => asDouble.get(i).asInstanceOf[A]
      case LongTag => asLong.get(i).asInstanceOf[A]
      case IntTag => asInt.get(i).asInstanceOf[A]
      case ShortTag => asShort.get(i).asInstanceOf[A]
      case ByteTag => asByte.get(i).asInstanceOf[A]
      case _ => error(s"NativeArray[${Numerical[A].show}] is not supported")
    }
  }

  def get(array: Array[A]): NativeArray[A] = get(array, 0, array.length)

  def get(array: Array[A], offset: Int, length: Int): NativeArray[A] =
    Numerical[A].tag match {
      case FloatTag => asFloat.get(array.asInstanceOf[Array[Float]], offset, length); this;
      case DoubleTag => asDouble.get(array.asInstanceOf[Array[Double]], offset, length); this;
      case LongTag => asLong.get(array.asInstanceOf[Array[Long]], offset, length); this;
      case IntTag => asInt.get(array.asInstanceOf[Array[Int]], offset, length); this;
      case ShortTag => asShort.get(array.asInstanceOf[Array[Short]], offset, length); this;
      case ByteTag => asByte.get(array.asInstanceOf[Array[Byte]], offset, length); this;
      case _ => error(s"NativeArray[${Numerical[A].show}] is not supported")
    }

  def put(i: Int, v: A): NativeArray[A] = {
    require(i < size, s"index $i is out of bound: [0, $size]")
    Numerical[A].tag match {
      case FloatTag => asFloat.put(i.toLong, v.asInstanceOf[Float]); this;
      case DoubleTag => asDouble.put(i.toLong, v.asInstanceOf[Double]); this;
      case LongTag => asLong.put(i.toLong, v.asInstanceOf[Long]); this;
      case IntTag => asInt.put(i.toLong, v.asInstanceOf[Int]); this;
      case ShortTag => asShort.put(i.toLong, v.asInstanceOf[Short]); this;
      case ByteTag => asByte.put(i.toLong, v.asInstanceOf[Byte]); this;
      case _ => error(s"NativeArray[${Numerical[A].show}] is not supported")
    }
  }

  def putAll(array: Array[A]): NativeArray[A] = put(array, 0, array.length)

  def put(array: Array[A], offset: Int, length: Int): NativeArray[A] = {
    val targetSize = length - offset
    require(targetSize <= size, s"allowed array size is $size but tried to put $targetSize")
    Numerical[A].tag match {
      case FloatTag => asFloat.put(array.asInstanceOf[Array[Float]], offset, length); this;
      case DoubleTag => asDouble.put().put(array.asInstanceOf[Array[Double]], offset, length); this;
      case LongTag => asLong.put(array.asInstanceOf[Array[Long]], offset, length); this;
      case IntTag => asInt.put(array.asInstanceOf[Array[Int]], offset, length); this;
      case ShortTag => asShort.put(array.asInstanceOf[Array[Short]], offset, length); this;
      case ByteTag => asByte.put(array.asInstanceOf[Array[Byte]], offset, length); this;
      case _ => error(s"NativeArray[${Numerical[A].show}] is not supported")
    }
  }

  def toArray: Array[A] = {
    val array: Array[A] = Array.ofDim[A](size)(Numerical[A].classTag)
    for (i <- array.indices) {
      array(i) = get(i)
    }
    array
  }

  def toStream: Stream[A] = {
    def next(index: Int): Stream[A] = {
      if (size == index) Stream.empty
      else get(index) #:: next(index + 1)
    }
    next(0)
  }

  override def close(): Unit = pointer.close()

  override def hashCode(): Int = toArray.foldLeft(1)((acc, a) => 31 * acc + a.hashCode())

  override def equals(obj: Any): Boolean = obj match {
    case other: NativeArray[A] => other.toArray sameElements toArray
    case _ => false
  }

  override def toString: String = s"NativeArray[${Numerical[A].show}](capacity=$capacity, position=$position, limit=$limit, address=${pointer.address()})" + show()

  def show(n: Int = 20): String = {
    val elements = toStream.take(n).mkString(", ")
    "[" + elements + (if (n < limit) "..." else "") + "]"
  }
}

object NativeArray extends TFTypeInstances {

  loadNative()

  def apply[@sp A: Numerical](size: Int): NativeArray[A] =
    Numerical[A].tag match {
      case FloatTag => new NativeArray[A](new FloatPointer(size.toLong))
      case DoubleTag => new NativeArray[A](new DoublePointer(size.toLong))
      case LongTag => new NativeArray[A](new LongPointer(size.toLong))
      case IntTag => new NativeArray[A](new IntPointer(size.toLong))
      case ShortTag => new NativeArray[A](new ShortPointer(size.toLong))
      case ByteTag => new NativeArray[A](new BytePointer(size.toLong))
      case _ => error(s"NativeArray[${Numerical[A].show}] is not supported")
    }

  def apply[@sp A: Numerical](array: A*): NativeArray[A] = {
    NativeArray[A](array.length).putAll(array.toArray(Numerical[A].classTag))
  }

  def apply[@sp A: Numerical](array: Array[A]): NativeArray[A] = {
    NativeArray[A](array.length).putAll(array)
  }

  // ## Implicit conversions ##
  // TPointer -> NativeArray[T]
  // XPointer -> Array[T]
  // NativeArray[T] -> Array[T]
  // Array[T] -> NativeArray[T]
  // Array[T] -> TPointer

  implicit def pointerArrayToArray[A: Numerical](pointer: NativeArray[A]): Array[A] = pointer.toArray
  implicit def arrayToPointerArray[A: Numerical](array: Array[A]): NativeArray[A] = NativeArray(array)

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
