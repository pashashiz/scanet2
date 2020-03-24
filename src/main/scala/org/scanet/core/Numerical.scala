package org.scanet.core

import org.bytedeco.tensorflow.global.tensorflow._
import simulacrum.typeclass

import scala.reflect.ClassTag

@typeclass trait Numerical[A] {
  def tag: Int
  def classTag: ClassTag[A]
  def show: String = classTag.toString()
}

object Numerical {
  val FloatTag: Int = DT_FLOAT
  val DoubleTag: Int = DT_DOUBLE
  val LongTag: Int = DT_INT64
  val IntTag: Int = DT_INT32
  val ShortTag: Int = DT_INT16
  val ByteTag: Int = DT_INT8
}

trait TFTypeInstances {
  implicit def floatTFType: Numerical[Float] = new Numerical[Float] {
    override def tag: Int = Numerical.FloatTag
    override def classTag: ClassTag[Float] = scala.reflect.classTag[Float]
  }
  implicit def doubleTFType: Numerical[Double] = new Numerical[Double] {
    override def tag: Int = Numerical.DoubleTag
    override def classTag: ClassTag[Double] = scala.reflect.classTag[Double]
  }
  implicit def longTFType: Numerical[Long] = new Numerical[Long] {
    override def tag: Int = Numerical.LongTag
    override def classTag: ClassTag[Long] = scala.reflect.classTag[Long]
  }
  implicit def intTFType: Numerical[Int] = new Numerical[Int] {
    override def tag: Int = Numerical.IntTag
    override def classTag: ClassTag[Int] = scala.reflect.classTag[Int]
  }
  implicit def shortTFType: Numerical[Short] = new Numerical[Short] {
    override def tag: Int = Numerical.ShortTag
    override def classTag: ClassTag[Short] = scala.reflect.classTag[Short]
  }
  implicit def byteTFType: Numerical[Byte] = new Numerical[Byte] {
    override def tag: Int = Numerical.ByteTag
    override def classTag: ClassTag[Byte] = scala.reflect.classTag[Byte]
  }
}