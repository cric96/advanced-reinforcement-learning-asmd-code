package it.unibo.examples

import it.unibo.model.core.abstractions.{AI, MultiAgentEnvironment, Scheduler}
import cats.{Align, Applicative, Foldable}
import cats.syntax.align.{*, given}
import cats.implicits.*
import it.unibo.view.Render

class Simulation[State, Action](using scheduler: Scheduler)(
    val environment: MultiAgentEnvironment[State, Action],
    renderer: Render[State] = Render.empty[State]
):
  import environment.*
  def simulate(
      episodes: Int,
      episodeLength: Int,
      agents: Seq[AI.Agent[State, Action]],
      onEpisodeEnd: Seq[Double] => Unit = _ => (),
      onStep: Seq[Double] => Unit = _ => ()
  ): Unit =
    scheduler.reset()
    for episode <- 0 to episodes do
      agents.foreach(_.reset())
      var totalRewards = agents.map(_ => 0.0)
      environment.reset()
      for _ <- 0 to episodeLength do
        val currentState = environment.state
        renderer.render(currentState)
        val actions = agents.map(_.act(currentState))
        val rewards = environment.act(actions)
        totalRewards = totalRewards.zip(rewards).map(_ + _)
        val nextState = environment.state
        val actionAndRewards = actions.zip(rewards)
        agents.zip(actionAndRewards).foreach { case (agent, (action, reward)) =>
          agent.record(currentState, action, reward, nextState)
        }
        onStep(rewards)
        scheduler.tickStep()
      scheduler.tickEpisode()
      onEpisodeEnd(totalRewards)
      println(s"Episode $episode, statistics: $totalRewards")

  def simulateCentralController(
      episodes: Int,
      episodeLength: Int,
      agent: AI.Agent[State, Seq[Action]]
  ): Unit =
    val space = agent.act(environment.state)
    scheduler.reset()
    for episode <- 0 to episodes do
      agent.reset()
      var totalRewards = space.map(_ => 0.0)
      environment.reset()
      for _ <- 0 to episodeLength do
        val currentState = environment.state
        renderer.render(currentState)
        val actions = agent.act(currentState)
        val rewards = environment.act(actions)
        totalRewards = totalRewards.zip(rewards).map(_ + _)
        val nextState = environment.state
        agent.record(state, actions, rewards.sumAll, nextState)
        scheduler.tickStep()
      scheduler.tickEpisode()
      println(s"Episode $episode, statistics: $totalRewards")
