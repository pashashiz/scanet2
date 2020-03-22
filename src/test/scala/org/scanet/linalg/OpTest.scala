package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scanet.linalg.Op.const

class OpTest extends AnyFlatSpec with Matchers {

  "op example" should "work" in {
    val expr = const("a", 5.0f)
    val session = new Session()
    val tensor: Tensor[Float] = session.run(expr)
    println(tensor.show())
  }

  "resulting tensor" should "be specialized" in {
    val session = new Session()
    val tensor: Tensor[Float] = session.run(const("a", 5.0f))
    tensor.getClass.getName should endWith("Tensor$mcF$sp")
  }
}
