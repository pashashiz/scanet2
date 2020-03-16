package org.scanet
import org.bytedeco.javacpp.LongPointer
import org.bytedeco.tensorflow.{LongVector, TensorShape, Tensor => NativeTensor}

import scala.annotation.tailrec
import scala.language.implicitConversions

package object core {

  def loadNative(): Unit = {
    Class.forName("org.bytedeco.tensorflow.Tensor")
  }

  def error(message: String): Nothing = throw new RuntimeException(message)

//  implicit def longVectorToLongList(vector: LongVector): List[Long] = {
//    @tailrec
//    def iter(next: Int, acc: List[Long]): List[Long] = {
//      if (vector.capacity() == next) acc
//      else iter(next + 1, vector.get(next) :: acc)
//    }
//    iter(0, Nil)
//  }
//
//  implicit def longListToLongVector(list: List[Long]): LongVector = {
//    val vector = new LongVector()
//    for ((v, i) <- list.zipWithIndex) {
//      vector.put(i, v)
//    }
//    vector
//  }
}
