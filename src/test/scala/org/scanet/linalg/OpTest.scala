package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scanet.linalg.Op.const

class OpTest extends AnyFlatSpec with Matchers {

  "op example" should "work" in {
    val expr = const("a", 5.0f)
    val tensor: Tensor[Float] = new Session().run(expr)
    println(tensor.value.get)
  }
}
