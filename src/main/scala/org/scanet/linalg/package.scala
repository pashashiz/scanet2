package org.scanet

import cats.data.State
import org.bytedeco.tensorflow.{Output => NativeOutput}

package object linalg {

  type Compiler = State[Context, NativeOutput]

}
