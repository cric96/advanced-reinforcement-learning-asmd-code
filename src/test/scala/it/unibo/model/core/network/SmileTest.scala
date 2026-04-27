package it.unibo.model.core.network

import org.scalatest.flatspec.AnyFlatSpec
import smile.deep.tensor.Tensor
import smile.deep.layer.*

class SmileTest extends AnyFlatSpec:

  "Tensor" should "create and perform basic operations" in:
    val t = Tensor.zeros(3, 4)
    assert(t.shape().sameElements(Array(3L, 4L)))
    t.close()

  "SequentialBlock" should "forward pass correctly" in:
    val network = new SequentialBlock(
      Layer.relu(4, 8),
      Layer.linear(8, 2)
    )
    val input = Tensor.randn(1, 4)
    val output = network.forward(input)
    assert(output.shape().sameElements(Array(1L, 2L)))
    input.close()
    output.close()

  "DQN" should "create and forward pass" in:
    val dqn = DQN(4, 16, 2)
    val input = Tensor.randn(1, 4)
    val output = dqn.forward(input)
    assert(output.shape().sameElements(Array(1L, 2L)))
    input.close()
    output.close()
