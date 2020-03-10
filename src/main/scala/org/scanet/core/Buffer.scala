package org.scanet.core

import java.nio.{ByteBuffer, DoubleBuffer, FloatBuffer, IntBuffer, LongBuffer, ShortBuffer, Buffer => JavaBuffer}

import org.scanet.core.TypeTags._

import scala.reflect.ClassTag
import scala.reflect._

class Buffer[A: ClassTag] private (original: JavaBuffer) extends Comparable[Buffer[A]] {

  private def asFloat: FloatBuffer = original.asInstanceOf[FloatBuffer]
  private def asDouble: DoubleBuffer = original.asInstanceOf[DoubleBuffer]
  private def asLong: LongBuffer = original.asInstanceOf[LongBuffer]
  private def asInt: IntBuffer = original.asInstanceOf[IntBuffer]
  private def asShort: ShortBuffer = original.asInstanceOf[ShortBuffer]
  private def asByte: ByteBuffer = original.asInstanceOf[ByteBuffer]

  def slice: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.slice(); this
    case DoubleTag => asDouble.slice(); this
    case LongTag => asLong.slice(); this
    case IntTag => asInt.slice(); this
    case ShortTag => asShort.slice(); this
    case ByteTag => asByte.slice(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def duplicate: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.duplicate(); this
    case DoubleTag => asDouble.duplicate(); this
    case LongTag => asLong.duplicate(); this
    case IntTag => asInt.duplicate(); this
    case ShortTag => asShort.duplicate(); this
    case ByteTag => asByte.duplicate(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def asReadOnlyBuffer: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.asReadOnlyBuffer(); this
    case DoubleTag => asDouble.asReadOnlyBuffer(); this
    case LongTag => asLong.asReadOnlyBuffer(); this
    case IntTag => asInt.asReadOnlyBuffer(); this
    case ShortTag => asShort.asReadOnlyBuffer(); this
    case ByteTag => asByte.asReadOnlyBuffer(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def get: A = classTag[A] match {
    case FloatTag => asFloat.get().asInstanceOf[A]
    case DoubleTag => asDouble.get().asInstanceOf[A]
    case LongTag => asLong.get().asInstanceOf[A]
    case IntTag => asInt.get().asInstanceOf[A]
    case ShortTag => asShort.get().asInstanceOf[A]
    case ByteTag => asByte.get().asInstanceOf[A]
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def put(f: A): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.put(f.asInstanceOf[Float]); this
    case DoubleTag => asDouble.put(f.asInstanceOf[Double]); this
    case LongTag => asLong.put(f.asInstanceOf[Long]); this
    case IntTag => asInt.put(f.asInstanceOf[Int]); this
    case ShortTag => asShort.put(f.asInstanceOf[Short]); this
    case ByteTag => asByte.put(f.asInstanceOf[Byte]); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def get(index: Int): A = classTag[A] match {
    case FloatTag => asFloat.get(index).asInstanceOf[A]
    case DoubleTag => asDouble.get(index).asInstanceOf[A]
    case LongTag => asLong.get(index).asInstanceOf[A]
    case IntTag => asInt.get(index).asInstanceOf[A]
    case ShortTag => asShort.get(index).asInstanceOf[A]
    case ByteTag => asByte.get(index).asInstanceOf[A]
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def put(index: Int, f: A): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.put(index, f.asInstanceOf[Float]); this
    case DoubleTag => asDouble.put(index, f.asInstanceOf[Double]); this
    case LongTag => asLong.put(index, f.asInstanceOf[Long]); this
    case IntTag => asInt.put(index, f.asInstanceOf[Int]); this
    case ShortTag => asShort.put(index, f.asInstanceOf[Short]); this
    case ByteTag => asByte.put(index, f.asInstanceOf[Byte]); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def get(dst: Array[A]): Buffer[A] = get(dst, 0, dst.length)

  def put(src: Buffer[A]): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.put(src.asFloat); this
    case DoubleTag => asDouble.put(src.asDouble); this
    case LongTag => asLong.put(src.asLong); this
    case IntTag => asInt.put(src.asInt); this
    case ShortTag => asShort.put(src.asShort); this
    case ByteTag => asByte.put(src.asByte); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def put(src: Array[A], offset: Int, length: Int): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.put(src.asInstanceOf[Array[Float]], offset, length); this
    case DoubleTag => asDouble.put(src.asInstanceOf[Array[Double]], offset, length); this
    case LongTag => asLong.put(src.asInstanceOf[Array[Long]], offset, length); this
    case IntTag => asInt.put(src.asInstanceOf[Array[Int]], offset, length); this
    case ShortTag => asShort.put(src.asInstanceOf[Array[Short]], offset, length); this
    case ByteTag => asByte.put(src.asInstanceOf[Array[Byte]], offset, length); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  // todo, need to have put name, maybe after @spec
  def putArray(src: Array[A]): Buffer[A] = put(src, 0, src.length)

  def compact: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.compact(); this
    case DoubleTag => asDouble.compact(); this
    case LongTag => asLong.compact(); this
    case IntTag => asInt.compact(); this
    case ShortTag => asShort.compact(); this
    case ByteTag => asByte.compact(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def get(dst: Array[A], offset: Int, length: Int): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.get(dst.asInstanceOf[Array[Float]], offset, length); this
    case DoubleTag => asDouble.get(dst.asInstanceOf[Array[Double]], offset, length); this
    case LongTag => asLong.get(dst.asInstanceOf[Array[Long]], offset, length); this
    case IntTag => asInt.get(dst.asInstanceOf[Array[Int]], offset, length); this
    case ShortTag => asShort.get(dst.asInstanceOf[Array[Short]], offset, length); this
    case ByteTag => asByte.get(dst.asInstanceOf[Array[Byte]], offset, length); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def capacity: Int = classTag[A] match {
    case FloatTag => asFloat.capacity()
    case DoubleTag => asDouble.capacity()
    case LongTag => asLong.capacity()
    case IntTag => asInt.capacity()
    case ShortTag => asShort.capacity()
    case ByteTag => asByte.capacity()
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def position: Int = classTag[A] match {
    case FloatTag => asFloat.position()
    case DoubleTag => asDouble.position()
    case LongTag => asLong.position()
    case IntTag => asInt.position()
    case ShortTag => asShort.position()
    case ByteTag => asByte.position()
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def position(newPosition: Int): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.position(newPosition); this
    case DoubleTag => asDouble.position(newPosition); this
    case LongTag => asLong.position(newPosition); this
    case IntTag => asInt.position(newPosition); this
    case ShortTag => asShort.position(newPosition); this
    case ByteTag => asByte.position(newPosition); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def limit: Int = classTag[A] match {
    case FloatTag => asFloat.limit()
    case DoubleTag => asDouble.limit()
    case LongTag => asLong.limit()
    case IntTag => asInt.limit()
    case ShortTag => asShort.limit()
    case ByteTag => asByte.limit()
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def limit(newLimit: Int): Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.limit(newLimit); this
    case DoubleTag => asDouble.limit(newLimit); this
    case LongTag => asLong.limit(newLimit); this
    case IntTag => asInt.limit(newLimit); this
    case ShortTag => asShort.limit(newLimit); this
    case ByteTag => asByte.limit(newLimit); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def mark: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.mark(); this
    case DoubleTag => asDouble.mark(); this
    case LongTag => asLong.mark(); this
    case IntTag => asInt.mark(); this
    case ShortTag => asShort.mark(); this
    case ByteTag => asByte.mark(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def reset: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.reset(); this
    case DoubleTag => asDouble.reset(); this
    case LongTag => asLong.reset(); this
    case IntTag => asInt.reset(); this
    case ShortTag => asShort.reset(); this
    case ByteTag => asByte.reset(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def clear: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.clear(); this
    case DoubleTag => asDouble.clear(); this
    case LongTag => asLong.clear(); this
    case IntTag => asInt.clear(); this
    case ShortTag => asShort.clear(); this
    case ByteTag => asByte.clear(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def flip: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.flip(); this
    case DoubleTag => asDouble.flip(); this
    case LongTag => asLong.flip(); this
    case IntTag => asInt.flip(); this
    case ShortTag => asShort.flip(); this
    case ByteTag => asByte.flip(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def rewind: Buffer[A] = classTag[A] match {
    case FloatTag => asFloat.rewind(); this
    case DoubleTag => asDouble.rewind(); this
    case LongTag => asLong.rewind(); this
    case IntTag => asInt.rewind(); this
    case ShortTag => asShort.rewind(); this
    case ByteTag => asByte.rewind(); this
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def remaining: Int = classTag[A] match {
    case FloatTag => asFloat.remaining()
    case DoubleTag => asDouble.remaining()
    case LongTag => asLong.remaining()
    case IntTag => asInt.remaining()
    case ShortTag => asShort.remaining()
    case ByteTag => asByte.remaining()
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def hasRemaining: Boolean = classTag[A] match {
    case FloatTag => asFloat.hasRemaining
    case DoubleTag => asDouble.hasRemaining
    case LongTag => asLong.hasRemaining
    case IntTag => asInt.hasRemaining
    case ShortTag => asShort.hasRemaining
    case ByteTag => asByte.hasRemaining
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def isReadOnly: Boolean = classTag[A] match {
    case FloatTag => asFloat.isReadOnly
    case DoubleTag => asDouble.isReadOnly
    case LongTag => asLong.isReadOnly
    case IntTag => asInt.isReadOnly
    case ShortTag => asShort.isReadOnly
    case ByteTag => asByte.isReadOnly
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def hasArray: Boolean = classTag[A] match {
    case FloatTag => asFloat.hasArray
    case DoubleTag => asDouble.hasArray
    case LongTag => asLong.hasArray
    case IntTag => asInt.hasArray
    case ShortTag => asShort.hasArray
    case ByteTag => asByte.hasArray
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def array: Array[A] = classTag[A] match {
    case FloatTag => asFloat.array().asInstanceOf[Array[A]]
    case DoubleTag => asDouble.array().asInstanceOf[Array[A]]
    case LongTag => asLong.array().asInstanceOf[Array[A]]
    case IntTag => asInt.array().asInstanceOf[Array[A]]
    case ShortTag => asShort.array().asInstanceOf[Array[A]]
    case ByteTag => asByte.array().asInstanceOf[Array[A]]
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def arrayOffset: Int = classTag[A] match {
    case FloatTag => asFloat.arrayOffset
    case DoubleTag => asDouble.arrayOffset
    case LongTag => asLong.arrayOffset
    case IntTag => asInt.arrayOffset
    case ShortTag => asShort.arrayOffset
    case ByteTag => asByte.arrayOffset
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  def isDirect: Boolean = classTag[A] match {
    case FloatTag => asFloat.isDirect
    case DoubleTag => asDouble.isDirect
    case LongTag => asLong.isDirect
    case IntTag => asInt.isDirect
    case ShortTag => asShort.isDirect
    case ByteTag => asByte.isDirect
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  override def toString: String = classTag[A] match {
    case FloatTag => asFloat.toString
    case DoubleTag => asDouble.toString
    case LongTag => asLong.toString
    case IntTag => asInt.toString
    case ShortTag => asShort.toString
    case ByteTag => asByte.toString
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  override def hashCode: Int = classTag[A] match {
    case FloatTag => asFloat.hashCode
    case DoubleTag => asDouble.hashCode
    case LongTag => asLong.hashCode
    case IntTag => asInt.hashCode
    case ShortTag => asShort.hashCode
    case ByteTag => asByte.hashCode
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  override def equals(ob: Any): Boolean = classTag[A] match {
    case FloatTag => asFloat.equals(ob)
    case DoubleTag => asDouble.equals(ob)
    case LongTag => asLong.equals(ob)
    case IntTag => asInt.equals(ob)
    case ShortTag => asShort.equals(ob)
    case ByteTag => asByte.equals(ob)
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }

  override def compareTo(that: Buffer[A]): Int = classTag[A] match {
    case FloatTag => asFloat.compareTo(that.asFloat)
    case DoubleTag => asDouble.compareTo(that.asDouble)
    case LongTag => asLong.compareTo(that.asLong)
    case IntTag => asInt.compareTo(that.asInt)
    case ShortTag => asShort.compareTo(that.asShort)
    case ByteTag => asByte.compareTo(that.asByte)
    case _ => error(s"Buffer[${classTag[A]}] is not supported")
  }
}

object Buffer {

  def main(args: Array[String]): Unit = {
    //println(Buffer.allocate[Float](1))
    println(Buffer.wrap(Array(1, 2, 3)).get)
  }

  def apply[A: ClassTag](original: JavaBuffer): Buffer[A] = new Buffer[A](original)
  def apply(original: FloatBuffer): Buffer[Float] = new Buffer(original)
  def apply(original: DoubleBuffer): Buffer[Double] = new Buffer(original)
  def apply(original: LongBuffer): Buffer[Long] = new Buffer(original)
  def apply(original: IntBuffer): Buffer[Int] = new Buffer(original)
  def apply(original: ShortBuffer): Buffer[Short] = new Buffer(original)
  def apply(original: ByteBuffer): Buffer[Byte] = new Buffer(original)

  def allocate[A: ClassTag](capacity: Int): Buffer[A] = {
     classTag[A] match {
      case FloatTag => Buffer(FloatBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case DoubleTag => Buffer(DoubleBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case LongTag => Buffer(LongBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case IntTag => Buffer(IntBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case ShortTag => Buffer(ShortBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case ByteTag => Buffer(ByteBuffer.allocate(capacity)).asInstanceOf[Buffer[A]]
      case _ => error(s"Buffer[${classTag[A]}] is not supported")
    }
  }

  def wrap[A: ClassTag](array: Array[A], offset: Int, length: Int): Buffer[A] = {
    classTag[A] match {
      case FloatTag => Buffer(FloatBuffer.wrap(array.asInstanceOf[Array[Float]], offset, length)).asInstanceOf[Buffer[A]]
      case DoubleTag => Buffer(DoubleBuffer.wrap(array.asInstanceOf[Array[Double]], offset, length)).asInstanceOf[Buffer[A]]
      case LongTag => Buffer(LongBuffer.wrap(array.asInstanceOf[Array[Long]], offset, length)).asInstanceOf[Buffer[A]]
      case IntTag => Buffer(IntBuffer.wrap(array.asInstanceOf[Array[Int]], offset, length)).asInstanceOf[Buffer[A]]
      case ShortTag => Buffer(ShortBuffer.wrap(array.asInstanceOf[Array[Short]], offset, length)).asInstanceOf[Buffer[A]]
      case ByteTag => Buffer(ByteBuffer.wrap(array.asInstanceOf[Array[Byte]], offset, length)).asInstanceOf[Buffer[A]]
      case _ => error(s"Buffer[${classTag[A]}] is not supported")
    }
  }

  def wrap[A: ClassTag](array: Array[A]): Buffer[A] = wrap(array, 0, array.length)

}
