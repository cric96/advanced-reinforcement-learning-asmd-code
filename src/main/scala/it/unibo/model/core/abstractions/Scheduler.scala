package it.unibo.model.core.abstractions

import it.unibo.model.core.abstractions.Scheduler.SchedulerListener

/** A global entity (during a simulation) that expresses the evolution of time. A simulation, is composed by two loop.
  * a) inner loop => steps. It expresses the evolution for a single simulation b) outer loop => episodes: It expresses
  * how many times one simulation run is performed In this case, we suppose that the episodes evolution is mainly
  * sequential.
  */
trait Scheduler:
  /** current episode count */
  def episode: Int

  /** current step count */
  def step: Int

  /** total ticks performed by a simulator */
  def totalTicks: Int

  /** progress the scheduler by one episode => it will restart the ticks count */
  def tickEpisode(): Unit

  /** progress the scheduler by one tick */
  def tickStep(): Unit

  /** listeners could be attached to the scheduler (e.g., temporal variable that changes over the episodes) */
  def attachListener(listener: SchedulerListener): Unit
  def reset(): Unit

object Scheduler:
  private class MutableScheduler extends Scheduler:
    private var listeners: List[SchedulerListener] = List.empty
    var episode: Int = 0
    var step: Int = 0
    var totalTicks: Int = 0

    override def tickStep(): Unit =
      listeners.foreach(_.onStep(step))
      step += 1
      totalTicks += 1

    override def tickEpisode(): Unit =
      listeners.foreach(_.onEpisode(step))
      episode += 1
      totalTicks += 1
      step = 0

    override def reset(): Unit =
      episode = 0
      totalTicks = 0
      step = 0

    override def attachListener(listener: SchedulerListener): Unit = listeners = listener :: listeners

  /** Listener used to be notified about episode and step changes */
  sealed trait SchedulerListener:
    def onStep(step: Int): Unit = {}
    def onEpisode(step: Int): Unit = {}

  def apply(): Scheduler = MutableScheduler()

  extension (scheduler: Scheduler)
    def eachStep(action: Int => Unit): Unit = scheduler.attachListener(
      new SchedulerListener:
        override def onStep(step: Int): Unit = action(step)
    )

    def eachEpisode(action: Int => Unit): Unit = scheduler.attachListener(
      new SchedulerListener:
        override def onEpisode(step: Int): Unit = action(step)
    )
