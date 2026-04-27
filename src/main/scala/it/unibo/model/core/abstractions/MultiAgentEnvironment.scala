package it.unibo.model.core.abstractions

/** A multiagent environment given an environment State and an Action agent space */
trait MultiAgentEnvironment[State, Action]:
  def state: State // current environment state
  def act(actions: Seq[Action]): Seq[Double]
  def reset(): Unit
