package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scanet.linalg.Op.{const, plus}
import org.scanet.syntax.core._

class OpSpec extends AnyFlatSpec with Matchers {

  "op example" should "work" in {
    val expr = const("a", 5.0f)
    val session = new Session()
    val tensor: Tensor[Float] = session.run(expr)
    println(tensor.show())
  }

  "plus" should "work" in {
    plus("c", const("a", 5.0f), const("b", 5.0f)).eval should be(Tensor.scalar(10.0f))
    // simplify to: (5.0f.tensor + 5.0f.tensor).eval === 10.0f.tensor
  }

  "resulting tensor" should "be specialized" in {
    val session = new Session()
    val tensor: Tensor[Float] = session.run(const("a", 5.0f))
    tensor.getClass.getName should endWith("Tensor$mcF$sp")
  }
}
