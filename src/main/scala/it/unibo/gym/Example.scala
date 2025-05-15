package it.unibo.gym

import it.unibo.model.core.abstractions.{DecayReference, Enumerable, Scheduler}
import it.unibo.model.core.learning.{DeepQAgent, ReplayBuffer}
import it.unibo.model.core.network.NeuralNetworkEncoding
import me.shadaj.scalapy.py

import scala.util.Random

// Type alias for the Gym environment
type Observation = py.Dynamic
type Reward = Double
type Done = Boolean
type Terminated = Boolean
type Info = py.Dynamic
type Truncated = Boolean
type ResetReturn = (Observation, Info)
type StepReturn = (Observation, Reward, Done, Truncated, Info)

trait Action:
  val value: Int
object Action:
  case object Left extends Action:
    override val value: Int = 0
  case object Right extends Action:
    override val value: Int = 1

  given Enumerable[Action] with
    def elements: IndexedSeq[Action] = IndexedSeq(Action.Left, Action.Right)

@main def main(): Unit =
  given scheduler: Scheduler = Scheduler()
  given random: Random = Random(42)
  given NeuralNetworkEncoding[py.Dynamic] with
    override def elements: Int = 4
    override def toSeq(elem: py.Dynamic): Seq[Double] = elem.as[Seq[Double]]
  val gym = py.module("gymnasium")
  val environment = gym.make("CartPole-v1")
  var (observationOld, _) = environment.reset().as[ResetReturn]
  val episodes = 200
  val episodeMaxLength = 200
  val renderEach = 10
  val memory: ReplayBuffer[py.Dynamic, Action] = ReplayBuffer.bounded(100000)
  val decay: DecayReference[Double] = DecayReference.exponentialDecay(0.9, 0.01).bounded(0.05)
  val agent =
    DeepQAgent(memory, decay, gamma = 0.99, learningRate = 0.0005, hiddenSize = 64, batchSize = 64, updateEach = 1000)
  agent.trainingMode()
  (0 to episodes).foreach: _ =>
    var episodeReward = 0.0
    var done = false
    while !done do
      val action = agent.behavioural(observationOld)
      var (observation, reward, doneCurrent, truncated, info) = environment.step(action.value).as[StepReturn]

      episodeReward += reward
      done = doneCurrent || scheduler.step >= episodeMaxLength || truncated
      agent.record(observationOld, action, reward, observation, done)
      if done then
        println(s"Episode finished after $episodeReward timesteps")
        println("EPSILON: " + decay)
        val (observationNew, _) = environment.reset().as[ResetReturn]
        observation = observationNew
      scheduler.tickStep()
      observationOld = observation
    scheduler.tickEpisode()

  val testEnv = gym.make("CartPole-v1", render_mode = "human")
  agent.testMode()

  var (observation, _) = testEnv.reset().as[ResetReturn]
  var done = false
  while !done do
    val action = agent.optimal(observation)
    val (observationNew, reward, doneCurrent, truncated, info) = testEnv.step(action.value).as[StepReturn]
    Thread.sleep(100)
    done = doneCurrent || truncated
    observation = observationNew
