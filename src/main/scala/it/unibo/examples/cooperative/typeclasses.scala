package it.unibo.examples.cooperative

import BoundedWorldEnvironment.RelativeState
import it.unibo.model.core.network.NeuralNetworkEncoding

class StateEncoding(agents: Int, maxBound: Float) extends NeuralNetworkEncoding[List[(Int, Int)]]:
  private val stateSpaceSize = 2
  override def elements: Int = agents * stateSpaceSize
  override def toSeq(elem: List[(Int, Int)]): Seq[Double] = elem.flatMap:
    case (l, r) =>
      List(l / maxBound, r / maxBound)

class RelativeStateEncoding(boundSize: Int) extends NeuralNetworkEncoding[RelativeState]:
  private val stateSpaceSize = 2
  override def elements: Int = stateSpaceSize
  override def toSeq(elem: RelativeState): Seq[Double] =
    List(elem.rowDiff.toDouble / boundSize, elem.colDiff.toDouble / boundSize)
