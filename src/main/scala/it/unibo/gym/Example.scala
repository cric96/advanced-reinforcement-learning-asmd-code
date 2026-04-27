package it.unibo.gym

import it.unibo.model.core.abstractions.{DecayReference, Scheduler}
import it.unibo.model.core.learning.{DeepQAgent, ReplayBuffer}

import scala.util.Random

case class EpsilonDecayConfig(start: Double = 0.9, end: Double = 0.01, min: Double = 0.05)

case class AgentConfig(
    gamma: Double = 0.99,
    learningRate: Double = 0.0005,
    hiddenSize: Int = 64,
    batchSize: Int = 64,
    updateEach: Int = 1000,
    memorySize: Int = 100000,
    epsilonDecay: EpsilonDecayConfig = EpsilonDecayConfig()
)

case class TrainingConfig(
    maxEpisodes: Int = 1500,
    episodeMaxLength: Int = 200,
    solveWindow: Int = 20,
    solveThreshold: Double = 200.0,
    logInterval: Int = 50
)

case class RenderConfig(
    renderEach: Int = 200,
    renderDelayMs: Long = 50L,
    testDisplayDurationMs: Long = 2000L
)

case class ExperimentConfig(
    training: TrainingConfig = TrainingConfig(),
    agent: AgentConfig = AgentConfig(),
    render: RenderConfig = RenderConfig(),
    randomSeed: Long = 42
)

object ExperimentConfig:
  val Default: ExperimentConfig = ExperimentConfig()

@main def main(): Unit =
  val config = ExperimentConfig.Default

  given scheduler: Scheduler = Scheduler()
  given random: Random = Random(config.randomSeed)

  val environment = CartPoleEnvironment()
  var observationOld = environment.reset()
  val memory: ReplayBuffer[CartPoleState, CartPoleAction] = ReplayBuffer.bounded(config.agent.memorySize)
  val decay: DecayReference[Double] =
    DecayReference.exponentialDecay(config.agent.epsilonDecay.start, config.agent.epsilonDecay.end)
      .bounded(config.agent.epsilonDecay.min)
  val agent = DeepQAgent(
    memory,
    decay,
    gamma = config.agent.gamma,
    learningRate = config.agent.learningRate,
    hiddenSize = config.agent.hiddenSize,
    batchSize = config.agent.batchSize,
    updateEach = config.agent.updateEach
  )
  agent.trainingMode()

  val renderer = CartPoleRenderer.show()
  val episodeRewards = scala.collection.mutable.ListBuffer[Double]()
  var solved = false

  var episodeCount = 0
  while episodeCount < config.training.maxEpisodes && !solved do
    var episodeReward = 0.0
    var done = false
    while !done do
      val action = agent.behavioural(observationOld)
      val result = environment.step(action)

      episodeReward += result.reward
      done = result.done || scheduler.step >= config.training.episodeMaxLength
      agent.record(observationOld, action, result.reward, result.state, done)

      if episodeCount % config.render.renderEach == 0 && !done then
        renderer.update(RenderState(episodeCount, episodeReward, result.state))
        Thread.sleep(config.render.renderDelayMs)

      if done then observationOld = environment.reset()
      else observationOld = result.state
      scheduler.tickStep()

    episodeRewards += episodeReward
    episodeCount += 1

    val recentRewards = episodeRewards.takeRight(config.training.solveWindow)
    if episodeRewards.size >= config.training.solveWindow && recentRewards.forall(_ >= config.training.solveThreshold) then
      solved = true
      val avg = f"${recentRewards.sum / config.training.solveWindow}%.1f"
      println(s"\nEnvironment solved in $episodeCount episodes! (avg reward over last ${config.training.solveWindow}: $avg)")
    else if episodeCount % config.training.logInterval == 0 then
      val avg = if recentRewards.nonEmpty then recentRewards.sum / recentRewards.size else 0.0
      val avgStr = f"$avg%.1f"
      val epsStr = f"${decay.value}%.3f"
      println(s"Episode $episodeCount, avg reward (last ${config.training.solveWindow}): $avgStr, EPSILON: $epsStr")

    scheduler.tickEpisode()

  println(s"\nTraining completed after $episodeCount episodes")

  RewardPlot().show(episodeRewards.toList)

  println("\n=== Testing ===")
  agent.testMode()

  var observation = environment.reset()
  var done = false
  var totalTestReward = 0.0
  while !done do
    val action = agent.optimal(observation)
    val result = environment.step(action)
    totalTestReward += result.reward
    done = result.done
    observation = result.state
    renderer.update(RenderState(episodeCount, totalTestReward, observation))
    Thread.sleep(config.render.renderDelayMs)

  println(s"\nTest episode finished with total reward: $totalTestReward")
  Thread.sleep(config.render.testDisplayDurationMs)
