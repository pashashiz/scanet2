package org.scanet.linalg

import java.util.UUID

import org.bytedeco.tensorflow.global.tensorflow.{Const, InitMain, TF_CHECK_OK}
import org.bytedeco.tensorflow.{Add, GraphDef, SessionOptions, StringTensorPairVector, StringVector, TensorVector, Input => NativeInput, Output => NativeOutput, Scope => NativeScope, Session => NativeSession}
import org.scanet.core.Numeric

import scala.{specialized => sp}

case class Context(scope: NativeScope, cache: Map[String, NativeOutput])

case class Op[A: Numeric](name: String, shape: Shape, inputs: List[Op[A]], compiler: (Context, List[NativeInput]) => NativeOutput) {

  require(name.nonEmpty, "name cannot be empty")

  val id: String = UUID.randomUUID().toString

  def compile(context: Context): (Context, NativeOutput) = {
    val (context1, outputs) = inputs.foldLeft((context, List[NativeOutput]()))(
      (acc, op) => {
        val (currentContext, outs) = acc
        val (newContext, out) = op.findOrCompile(currentContext)
        (newContext, out::outs)
      })
    val output = compiler.apply(context1, outputs.map(out => new NativeInput(out)))
    val context2 = context1.copy(cache = context1.cache + (id -> output))
    (context2, output)
  }

  def findOrCompile(context: Context): (Context, NativeOutput) = {
    context.cache.get(id)
      .map((context, _))
      .getOrElse {compile(context)}
  }

  override def toString: String = {
    val args = if (inputs.nonEmpty) s"(${inputs.mkString(", ")})" else ""
    name + args
  }

  def eval: Tensor[A] = {
    // todo: better session impl
    val session = new Session()
    val tensor: Tensor[A] = session.run(this)
    session.close
    tensor
  }
}

object Op {

  def const[A: Numeric](value: A): Op[A] =
    const(Tensor.scalar[A](value))

  def const[A: Numeric](name: String, value: A): Op[A] =
    const(name, Tensor.scalar[A](value))

  def const[A: Numeric](tensor: Tensor[A]): Op[A] = const("const", tensor)

  def const[A: Numeric](name: String = "const", tensor: Tensor[A]): Op[A] =
    Op(name, tensor.shape, Nil, (context, _) => {
      Const(context.scope.WithOpName(name), tensor)
    })

  def plus[A: Numeric](left: Op[A], right: Op[A]): Op[A] = plus("plus", left, right)

  def plus[A: Numeric](name: String, left: Op[A], right: Op[A]): Op[A] = {
    require(left.shape == right.shape, s"shape ${left.shape} does not equal to ${right.shape}")
    Op(name, left.shape, List(left, right), (context, inputs) => {
      new Add(
        context.scope.WithOpName(name),
        new NativeInput(inputs.head),
        new NativeInput(inputs(1))
      ).asOutput()
    })
  }
}

class Session {

  def run[@sp A1: Numeric](op: Op[A1]): Tensor[A1] = {
    InitMain("Scanet", null.asInstanceOf[Array[Int]], null)
    val scope = NativeScope.NewRootScope
    val (_, output) = op.compile(Context(scope, Map.empty))
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