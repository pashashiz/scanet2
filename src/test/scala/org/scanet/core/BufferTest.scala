package org.scanet.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BufferTest extends AnyFlatSpec with Matchers {

  "buffer" should "have toString" in {
    Buffer.wrap(Array(1, 2, 3)).position(1) should
      be("Buffer[Int](capacity=3, position=1, limit=3, direct=false)[2, 3]")
  }

  it should "return next element" in {
    Buffer.wrap(Array(1, 2, 3)).get should be(1)
  }
}
