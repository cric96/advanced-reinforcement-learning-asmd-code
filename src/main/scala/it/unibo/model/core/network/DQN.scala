package it.unibo.model.core.network

import smile.deep.layer.*
import smile.deep.tensor.Tensor

class DQN(input: Int, hidden: Int, output: Int):
  private val network = new SequentialBlock(
    Layer.relu(input, hidden),
    Layer.relu(hidden, hidden),
    Layer.linear(hidden, output)
  )

  private val model = new smile.deep.Model(network)

  def forward(input: Tensor): Tensor = network.forward(input)

  def asTorch(): org.bytedeco.pytorch.Module = network.asTorch()

  def asModel(): smile.deep.Model = model
