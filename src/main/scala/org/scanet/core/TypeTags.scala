package org.scanet.core

import scala.reflect.{ClassTag, classTag}

object TypeTags {
  val FloatTag: ClassTag[Float] = classTag[Float]
  val DoubleTag: ClassTag[Double] = classTag[Double]
  val LongTag: ClassTag[Long] = classTag[Long]
  val IntTag: ClassTag[Int] = classTag[Int]
  val ShortTag: ClassTag[Short] = classTag[Short]
  val ByteTag: ClassTag[Byte] = classTag[Byte]
}
