package org.scanet.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BufferTest extends AnyFlatSpec with Matchers {

  "float buffer" should "return next element" in {
    Buffer.wrap(Array(1, 2, 3)).get should be(1)
  }

  "buffer" should "have toString" in {
    println(Buffer.wrap(Array(1, 2, 3)))
  }
}
