package org.scanet.linalg

import org.bytedeco.tensorflow.global.tensorflow.{InitMain, TF_CHECK_OK}
import org.bytedeco.tensorflow.{GraphDef, SessionOptions, StringTensorPairVector, StringVector, TensorVector, Scope => NativeScope, Session => NativeSession}
import org.scanet.core.Numeric

import scala.{specialized => sp}

object Session {

  // (a + b).eval
  // (a, b).eval
  def run[@sp A1: Numeric](op: Op[A1]): Tensor[A1] = {
    InitMain("scanet", null.asInstanceOf[Array[Int]], null)
    val scope = NativeScope.NewRootScope
    // todo: make unique names
    val (_, output) = op.compile(Context(scope, Map.empty))
    val graph = new GraphDef
    TF_CHECK_OK(scope.ToGraphDef(graph))
    val session = new NativeSession(new SessionOptions)
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
       if (session != null)
         session.close()
    }
  }
}
