package it.unibo.model.core.learning

import it.unibo.model.core.network.{DQN, NeuralNetworkEncoding}
import it.unibo.model.core.abstractions.{AI, DecayReference, Enumerable, Scheduler}
import it.unibo.model.core.learning.Learner
import smile.deep.tensor.Tensor
import smile.deep.Optimizer
import smile.deep.Loss

import scala.util.Random
import scala.util.Using
import it.unibo.model.core.abstractions.DecayReference.*

class DeepQAgent[State, Action: Enumerable](
    memory: ReplayBuffer[State, Action],
    epsilon: Ref[Double],
    gamma: Double,
    learningRate: Ref[Double],
    hiddenSize: Int = 32,
    batchSize: Int = 32,
    updateEach: Int = 100
)(using stateEncoding: NeuralNetworkEncoding[State], random: Random, scheduler: Scheduler)
    extends AI.Agent[State, Action]
    with Learner[State, Action]:
  self =>
  private val nActions = Enumerable[Action].size
  private val stateSize = stateEncoding.elements
  private val targetNetwork = DQN(stateSize, hiddenSize, nActions)
  private val policyNetwork = DQN(stateSize, hiddenSize, nActions)

  def slave(): AI.Agent[State, Action] with Learner[State, Action] =
    new AI.Agent[State, Action] with Learner[State, Action]:
      override def improve(state: State, action: Action, reward: Double, nextState: State, done: Boolean): Unit =
        memory.insert(state, action, reward, nextState, done)
      val behavioural = self.behavioural
      val optimal = self.optimal

  private val smileOptimizer = Optimizer.SGD(policyNetwork.asModel(), learningRate.value)
  private val criterion = Loss.mse()

  val behavioural: State => Action = state =>
    if random.nextDouble() < epsilon.value then random.shuffle(Enumerable[Action]).head
    else actionFromNet(state, policyNetwork)

  val optimal: State => Action = state => actionFromNet(state, targetNetwork)

  override def improve(state: State, action: Action, reward: Double, nextState: State, done: Boolean): Unit =
    memory.insert(state, action, reward, nextState, done)
    val memorySample = memory.sample(batchSize)
    if memorySample.size == batchSize then
      val states = encodeStates(memorySample.map(_.state).toSeq)
      val actionIndices = memorySample.map(_.action).toSeq.map(Enumerable[Action].indexOf).toArray
      val rewards = memorySample.map(_.reward).map(_.toFloat).toArray
      val nextStates = encodeStates(memorySample.map(_.nextState).toSeq)
      val dones = memorySample.map(_.done).map(d => if d then 0f else 1f).toArray

      Using
        .Manager { use =>
          val statesTensor = use(Tensor.of(states).reshape(batchSize, stateSize))
          val actionsTensor = use(Tensor.of(actionIndices.map(_.toLong)).reshape(batchSize, 1))
          val rewardsTensor = use(Tensor.of(rewards))
          val nextStatesTensor = use(Tensor.of(nextStates).reshape(batchSize, stateSize))
          val maskTensor = use(Tensor.of(dones).reshape(batchSize, 1))

          val stateActionValue = use(policyNetwork.forward(statesTensor).gather(1, actionsTensor))

          val guard = Tensor.noGradGuard()
          val nextStateValues = use {
            try
              val nextQValues = targetNetwork.forward(nextStatesTensor)
              val bestActionIndices = nextQValues.argmax(1, false)
              nextQValues.gather(1, bestActionIndices.reshape(batchSize, 1)).mul(maskTensor)
            finally guard.close()
          }

          val expectedValue = use(nextStateValues.mul(gamma).add(rewardsTensor.reshape(batchSize, 1)))
          val loss = use(criterion.apply(stateActionValue, expectedValue))

          smileOptimizer.reset()
          loss.backward()
          smileOptimizer.step()

          if scheduler.totalTicks % updateEach == 0 then copyWeights(policyNetwork, targetNetwork)
        }
        .failed
        .foreach(throw _)

  private def encodeStates(states: Seq[State]): Array[Float] =
    states.flatMap(state => stateEncoding.toSeq(state).map(_.toFloat)).toArray

  private def copyWeights(from: DQN, to: DQN): Unit =
    val fromParams = from.asTorch().parameters()
    val toParams = to.asTorch().parameters()
    (0L until fromParams.size()).foreach { i =>
      Using(fromParams.get(i).data().clone()) { fromData =>
        toParams.get(i).data().copy_(fromData)
      }
    }

  private def actionFromNet(state: State, network: DQN): Action =
    val encoded = stateEncoding.toSeq(state).map(_.toFloat).toArray
    Using.Manager { use =>
      val inputTensor = use(Tensor.of(encoded).reshape(1, stateSize))
      val guard = Tensor.noGradGuard()
      val outputTensor = use {
        try network.forward(inputTensor)
        finally guard.close()
      }
      val actionIndex = outputTensor.argmax(1, false).intValue()
      Enumerable[Action].toList(actionIndex)
    }.get
