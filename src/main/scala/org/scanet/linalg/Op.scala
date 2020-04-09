package org.scanet.linalg

import org.bytedeco.tensorflow.{Add, GraphDef, Input, Scope, SessionOptions, StringTensorPairVector, StringVector, TensorVector, Session => NativeSession}
import org.bytedeco.tensorflow.global.tensorflow.{Const, InitMain, TF_CHECK_OK}
import org.scanet.core.Numeric
import org.scanet.linalg.Op.{NativeOutput, const}

import scala.{specialized => sp}

case class Context(scope: Scope)

case class Op[A: Numeric](name: Option[String],
                 inputs: List[Op[A]],
                 shape: Shape,
                 compiler: Context => NativeOutput) {
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

  type NativeOutput = org.bytedeco.tensorflow.Output

  def const[A: Numeric](name: String, value: A): Op[A] =
    const(name, Tensor.scalar[A](value))

  def const[A: Numeric](name: String, tensor: Tensor[A]): Op[A] = {
    Op(Some(name), Nil, Shape(), context => {
      Const(context.scope.WithOpName(name), tensor)
    })
  }

  def plus[A: Numeric](name: String, left: Op[A], right: Op[A]): Op[A] = {
    Op(Some(name), Nil, Shape(), context => {
      new Add(
        context.scope.WithOpName(name),
        new Input(left.compiler(context)),
        new Input(right.compiler(context))
      ).asOutput()
    })
  }
}

class Session {

  def run[@sp A1: Numeric](op: Op[A1]): Tensor[A1] = {
    InitMain("Scanet", null.asInstanceOf[Array[Int]], null)
    val scope = Scope.NewRootScope
    val output = op.compiler(Context(scope))
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