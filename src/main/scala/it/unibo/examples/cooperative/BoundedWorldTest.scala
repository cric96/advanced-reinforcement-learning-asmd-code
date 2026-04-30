package it.unibo.examples.cooperative

import cats.{Eval, Reducible}
import it.unibo.model.core.abstractions.Enumerable.*
import scala.util.Random
import cats.implicits.*
import it.unibo.examples.Simulation
import it.unibo.model.core.abstractions.{AI, DecayReference, Enumerable, Q, Scheduler}
import it.unibo.model.core.learning.{DeepQAgent, QAgent, ReplayBuffer}
import it.unibo.model.core.network.NeuralNetworkEncoding
import it.unibo.view.Render
object BoundedWorldTest:
  import BoundedWorldEnvironment.*

  val numAgents = 6
  val boundSize = 10
  val numActions = 5
  val trainingEpisodes = 1000
  val longTraining = 10000
  val testEpisodes = 10
  val stepsPerEpisode = 25
  val replayBufferSize = 3000
  val renderIntervalTest = 1
  val renderIntervalTraining = 100
  val frameDelayMs = 33

  private def printSpaceInfo(name: String, states: Long, actions: Long): Unit =
    println(s"[$name] States: $states, Actions: $actions, State-Action pairs: ${states * actions}")

  given Random = Random(42)
  given Enumerable[Seq[MovementAction]] = Enumerable.productOf(numAgents)
  given Enumerable[RelativeState] =
    (for
      r <- 0 until boundSize
      c <- 0 until boundSize
    yield RelativeState(r, c)).asEnumerable
  given NeuralNetworkEncoding[State] = StateEncoding(numAgents, boundSize)
  given NeuralNetworkEncoding[RelativeState] = RelativeStateEncoding(boundSize)
  given Scheduler = Scheduler()
  val environment = BoundedWorldEnvironment(numAgents, boundSize)
  val render = GridWorldRender(boundSize, renderIntervalTraining, frameDelayMs)
  val simulator = Simulation(environment, render)

  @main def sharedQ(): Unit =
    val learningRate = 0.2
    val gamma = 0.95
    val epsilonInitial = 0.9
    val epsilonDecay = 0.003
    val epsilonMin = 0.05

    val relativeStates = boundSize * boundSize
    printSpaceInfo("sharedQ", relativeStates, numActions)
    val same = Q.zeros[RelativeState, Action]
    val agents = environment.state.indices.map { i =>
      val qAgent = QAgent(
        same,
        learningRate,
        gamma,
        DecayReference.exponentialDecay(epsilonInitial, epsilonDecay).bounded(epsilonMin)
      )
      RelativeStateAgent(qAgent, i, boundSize)
    }
    agents.foreach(_.trainingMode())
    simulator.simulate(trainingEpisodes, stepsPerEpisode, agents)
    agents.foreach(_.testMode())
    render.renderEvery(renderIntervalTest)
    simulator.simulate(testEpisodes, stepsPerEpisode, agents)

  @main def independentLearner(): Unit =
    val learningRate = 0.2
    val gamma = 0.95
    val epsilonInitial = 0.9
    val epsilonDecay = 0.0005
    val epsilonMin = 0.05

    val relativeStates = boundSize * boundSize
    printSpaceInfo("independentLearner", relativeStates, numActions)
    val agents = environment.state.indices.map { i =>
      val qAgent =
        QAgent(
          Q.zeros[RelativeState, Action],
          learningRate,
          gamma,
          DecayReference.exponentialDecay(epsilonInitial, epsilonDecay).bounded(epsilonMin)
        )
      RelativeStateAgent(qAgent, i, boundSize)
    }
    agents.foreach(_.trainingMode())
    simulator.simulate(trainingEpisodes, stepsPerEpisode, agents)
    agents.foreach(_.testMode())
    render.renderEvery(renderIntervalTest)
    simulator.simulate(testEpisodes, stepsPerEpisode, agents)

  @main def centralController(): Unit =
    val learningRate = 0.05
    val gamma = 0.99
    val epsilonMin = 0.05

    val absStates = math.pow(boundSize.toDouble, 2 * numAgents).toLong
    val jointActions = math.pow(numActions.toDouble, numAgents).toLong
    printSpaceInfo("centralController", absStates, jointActions)
    val qTable = Q.zeros[State, Seq[Action]]
    val centralAgent = QAgent(qTable, learningRate, gamma, epsilonMin)
    centralAgent.trainingMode()
    simulator.simulateCentralController(trainingEpisodes, stepsPerEpisode, centralAgent)
    centralAgent.testMode()
    render.renderEvery(renderIntervalTest)
    simulator.simulateCentralController(testEpisodes, stepsPerEpisode, centralAgent)

  @main def deepQLearner(): Unit =
    val learningRate = 0.01
    val gamma = 0.99
    val epsilonInitial = 0.9
    val epsilonDecay = 0.01
    val epsilonMin = 0.01
    val batchSize = 32
    val targetUpdateFreq = 64

    val relativeStates = boundSize * boundSize
    printSpaceInfo("deepQLearner", relativeStates, numActions)
    val agents = environment.state.indices.map { i =>
      val memory: ReplayBuffer[RelativeState, Action] = ReplayBuffer.bounded(replayBufferSize)
      val qAgent = DeepQAgent(
        memory,
        DecayReference.exponentialDecay(epsilonInitial, epsilonDecay).bounded(epsilonMin),
        gamma,
        learningRate,
        batchSize,
        targetUpdateFreq
      )
      RelativeStateAgent(qAgent, i, boundSize)
    }
    agents.foreach(_.trainingMode())
    simulator.simulate(trainingEpisodes, stepsPerEpisode, agents)
    agents.foreach(_.testMode())
    render.renderEvery(renderIntervalTest)
    simulator.simulate(testEpisodes, stepsPerEpisode, agents)

  @main def sharedDeepQLearner(): Unit =
    val learningRate = 0.01
    val gamma = 0.99
    val epsilonInitial = 0.9
    val epsilonDecay = 0.001
    val epsilonMin = 0.01
    val batchSize = 32
    val targetUpdateFreq = 64

    val relativeStates = boundSize * boundSize
    printSpaceInfo("sharedDeepQLearner", relativeStates, numActions)
    val memory: ReplayBuffer[RelativeState, Action] = ReplayBuffer.bounded(replayBufferSize)
    val qLearner = DeepQAgent(
      memory,
      DecayReference.exponentialDecay(epsilonInitial, epsilonDecay).bounded(epsilonMin),
      gamma,
      learningRate,
      batchSize,
      targetUpdateFreq
    )
    val agents = environment.state.indices.map { i =>
      RelativeStateAgent(if i == 0 then qLearner else qLearner.slave(), i, boundSize)
    }
    agents.foreach(_.trainingMode())
    simulator.simulate(trainingEpisodes, stepsPerEpisode, agents)
    render.renderEvery(renderIntervalTest)
    agents.foreach(_.testMode())
    simulator.simulate(testEpisodes, stepsPerEpisode, agents)

  @main def centralControllerDeep(): Unit =
    val learningRate = 0.01
    val gamma = 0.99
    val epsilonInitial = 0.9
    val epsilonDecay = 0.001
    val epsilonMin = 0.05
    val batchSize = 64
    val targetUpdateFreq = 64
    val hiddenSize = 64

    val absStates = math.pow(boundSize.toDouble, 2 * numAgents).toLong
    val jointActions = math.pow(numActions.toDouble, numAgents).toLong
    printSpaceInfo("centralControllerDeep", absStates, jointActions)
    val memory: ReplayBuffer[State, Seq[Action]] = ReplayBuffer.bounded(replayBufferSize)
    val epsilon = DecayReference.exponentialDecay(epsilonInitial, epsilonDecay).bounded(epsilonMin)
    val learner = DeepQAgent(memory, epsilon, gamma, learningRate, hiddenSize, targetUpdateFreq)
    learner.trainingMode()
    simulator.simulateCentralController(longTraining, stepsPerEpisode, learner)
    learner.testMode()
    render.renderEvery(renderIntervalTest)
    simulator.simulateCentralController(testEpisodes, stepsPerEpisode, learner)
