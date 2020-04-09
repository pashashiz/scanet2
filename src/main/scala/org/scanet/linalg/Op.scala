package org.scanet.linalg

import cats.data.State
import org.bytedeco.tensorflow.global.tensorflow.{Const, InitMain, TF_CHECK_OK}
import org.bytedeco.tensorflow.{Add, GraphDef, SessionOptions, StringTensorPairVector, StringVector, TensorVector, Input => NativeInput, Output => NativeOutput, Scope => NativeScope, Session => NativeSession}
import org.scanet.core.Numeric
import org.scanet.linalg.Compiler.compiler

import scala.{specialized => sp}

case class Context(scope: NativeScope, cache: Map[String, NativeOutput])

object Compiler {

  def unit(out: NativeOutput): Compiler = State(ctx => (ctx, out))

  def compiler(id: String, f: NativeScope => NativeOutput): Compiler =
    State(ctx => {
      ctx.cache.get(id)
        .map((ctx, _))
        .getOrElse({
          val compiled = f(ctx.scope)
          val newCache = ctx.cache + (id -> compiled)
          (ctx.copy(cache = newCache), compiled)
        })
    })
}

case class Op[A: Numeric](name: Option[String],
                 inputs: List[Op[A]],
                 shape: Shape,
                 compiler: Compiler) {
  require(name.nonEmpty, "name cannot be empty")

  def eval: Tensor[A] = {
    // todo: better session impl
    val session = new Session()
    val tensor: Tensor[A] = session.run(this)
    session.close
    tensor
  }
}

object Op {

  def simple[A: Numeric](name: Option[String], inputs: List[Op[A]], shape: Shape,
                        compilerFn: NativeScope => NativeOutput): Op[A] = {
    Op(name, inputs, shape, compiler(name.get, compilerFn))
  }

  def defer[A: Numeric](name: Option[String], inputs: List[Op[A]], shape: Shape,
                        compiler: => Compiler): Op[A] = {
    Op(name, inputs, shape, compiler)
  }

  def const[A: Numeric](name: String, value: A): Op[A] =
    const(name, Tensor.scalar[A](value))

  def const[A: Numeric](name: String, tensor: Tensor[A]): Op[A] = {
    Op.simple(Some(name), Nil, Shape(), scope => {
      println(s"compiling const: $name")
      Const(scope.WithOpName(name), tensor)
    })
  }

  def plus[A: Numeric](name: String, left: Op[A], right: Op[A]): Op[A] = {
    // todo find shape
    Op.defer(Some(name), Nil, Shape(), {
      for {
        l <- left.compiler
        r <- right.compiler
        out <- compiler(name, scope => new Add(
          scope.WithOpName(name), new NativeInput(l), new NativeInput(r)).asOutput())
      } yield out
    })
  }

}

class Session {

  def run[@sp A1: Numeric](op: Op[A1]): Tensor[A1] = {
    InitMain("Scanet", null.asInstanceOf[Array[Int]], null)
    val scope = NativeScope.NewRootScope
    val output = op.compiler.runA(Context(scope, Map.empty)).value
    val graph = new GraphDef
    TF_CHECK_OK(scope.ToGraphDef(graph))
    val options = new SessionOptions
    val session = new NativeSession(options)
    try {
      TF_CHECK_OK(session.Create(graph))
      val outputs = new TensorVector
      TF_CHECK_OK(session.Run(
        new StringTensorPairVector,
        new StringVector(output.node.name),
        new StringVector,
        outputs))
      Tensor(outputs.get()(0))
    } finally {
      // if (session != null) session.close()
    }
  }

  def close: Void = {
    // todo
    null
  }
}