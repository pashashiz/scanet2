package org.scanet
import scala.language.implicitConversions

package object core {

  def loadNative(): Unit = {
    Class.forName("org.bytedeco.tensorflow.Tensor")
  }

  def error(message: String): Nothing = throw new RuntimeException(message)
}
