package org.scanet.linalg

import java.nio.{Buffer => JavaBuffer}
import org.bytedeco.tensorflow.{Tensor => JavaTensor}

import org.bytedeco.tensorflow.{GraphDef, Scope, Session => JavaSession, SessionOptions, StringTensorPairVector, StringVector, TensorVector}
import org.bytedeco.tensorflow.global.tensorflow.{Const, InitMain, TF_CHECK_OK}
import org.scanet.core.Buffer
import org.scanet.linalg.Op.NativeOutput

import scala.reflect.ClassTag

case class Context(scope: Scope)

// todo: specialize buffer
case class Tensor[A](shape: List[Int], value: Buffer[A])

case class Op[A](name: Option[String],
                 inputs: List[Op[A]],
                 shape: List[Int],
                 compiler: Context => NativeOutput) {
  require(name.nonEmpty, "name cannot be empty")
}

object Op {

  type NativeOutput = org.bytedeco.tensorflow.Output

  def const[A](name: String, value: A): Op[A] = {
    Op(Some(name), Nil, Nil, context => {
      // todo: figure out how to call overloaded method here ...
      Const(context.scope.WithOpName(name), value.asInstanceOf[Float])
    })
  }
}

class Session {
  def run[A1: ClassTag](op: Op[A1]): Tensor[A1] = {
    InitMain("ExampleAdd2", null.asInstanceOf[Array[Int]], null)
    val scope = Scope.NewRootScope
    val output = op.compiler(Context(scope))
    val graph = new GraphDef
    TF_CHECK_OK(scope.ToGraphDef(graph))
    val options = new SessionOptions
    val session = new JavaSession(options)
    try {
      TF_CHECK_OK(session.Create(graph))
      val outputs = new TensorVector
      TF_CHECK_OK(session.Run(
        new StringTensorPairVector,
        new StringVector(output.node.name),
        new StringVector,
        outputs))
      val results: Array[JavaTensor] = outputs.get()
      Buffer(results(0).createBuffer[JavaBuffer])
      Tensor(op.shape, Buffer[A1](results(0).createBuffer[JavaBuffer]))
    } finally {
      if (session != null) session.close()
    }
  }
}