package org.scanet.core

import java.util

import org.scalatest.flatspec.AnyFlatSpec

class PointerArrayTest extends AnyFlatSpec {

  "int pointer" should "return next element" in {
    println(NativeArray(1, 2, 3).get(1))
  }

  "long pointer" should "be converted into int pointer" in {
    val a = NativeArray(1L, 2L, 3L).to[Int]
    println(a.capacity)
    println(util.Arrays.toString(a.toArray))
  }

  "long pointer" should "be converted into int buffer" in {
    val a = NativeArray(1L, 2L, 3L).asBuffer[Int]
    println(a.capacity)
    println(a.get(0))
    println(a.get(1))
    println(a.get(2))
  }
}
