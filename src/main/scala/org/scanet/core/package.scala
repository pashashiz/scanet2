package org.scanet

package object core {

  def loadNative(): Unit = {
    Class.forName("org.bytedeco.tensorflow.Tensor")
  }

  def error(message: String): Nothing = throw new RuntimeException(message)
}
