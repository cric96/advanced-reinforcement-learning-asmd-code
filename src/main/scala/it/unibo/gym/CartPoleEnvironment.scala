package it.unibo.gym

import scala.util.Random
import it.unibo.model.core.network.NeuralNetworkEncoding
import it.unibo.model.core.abstractions.Enumerable

class CartPoleEnvironment(config: CartPoleConfig)(using random: Random):
  import CartPoleConstants.{RewardOnStep, RewardOnFailure}

  private val totalMass = config.massCart + config.massPole
  private val poleMassLength = config.massPole * config.length

  private var state: CartPoleState = CartPoleState.zero
  private var done: Boolean = false
  private var stepCount: Int = 0

  def reset(): CartPoleState =
    state = CartPoleState.randomInit
    done = false
    stepCount = 0
    state

  def step(action: CartPoleAction): StepResult =
    if done then return StepResult(state, RewardOnFailure, true)

    val force = CartPoleAction.force(action, config.forceMag)
    val newState = state.applyForce(
      force = force,
      gravity = config.gravity,
      totalMass = totalMass,
      poleMassLength = poleMassLength,
      length = config.length,
      tau = config.tau
    )

    state = newState
    stepCount += 1

    val isDone = state.isTerminal(config.xThreshold, config.thetaThresholdRadians, stepCount, config.maxSteps)
    done = isDone
    val reward = if isDone then RewardOnFailure else RewardOnStep

    StepResult(state, reward, isDone)

  def currentState: CartPoleState = state
  def isDone: Boolean = done

object CartPoleEnvironment:
  def apply(config: CartPoleConfig = CartPoleConfig.Default)(using random: Random): CartPoleEnvironment =
    new CartPoleEnvironment(config)

case class CartPoleState(
    position: Double,
    velocity: Double,
    angle: Double,
    angularVelocity: Double
):
  import CartPoleConstants.UniformRodInertiaFactor

  def applyForce(
      force: Double,
      gravity: Double,
      totalMass: Double,
      poleMassLength: Double,
      length: Double,
      tau: Double
  ): CartPoleState =
    val costheta = math.cos(angle)
    val sintheta = math.sin(angle)

    val temp = (force + poleMassLength * angularVelocity * angularVelocity * sintheta) / totalMass
    val thetaAcc = (gravity * sintheta - costheta * temp) /
      (length * (UniformRodInertiaFactor - poleMassLength * costheta * costheta / (totalMass * length)))
    val xAcc = temp - poleMassLength * thetaAcc * costheta / totalMass

    CartPoleState(
      position = position + tau * velocity,
      velocity = velocity + tau * xAcc,
      angle = angle + tau * angularVelocity,
      angularVelocity = angularVelocity + tau * thetaAcc
    )

  def isTerminal(xThreshold: Double, thetaThreshold: Double, stepCount: Int, maxSteps: Int): Boolean =
    math.abs(position) > xThreshold ||
      math.abs(angle) > thetaThreshold ||
      stepCount >= maxSteps

object CartPoleState:
  import CartPoleConstants.{InitNoiseRange, InitNoiseOffset}

  def zero: CartPoleState = CartPoleState(0.0, 0.0, 0.0, 0.0)

  def randomInit(using random: Random): CartPoleState =
    CartPoleState(
      position = random.nextDouble() * InitNoiseRange - InitNoiseOffset,
      velocity = random.nextDouble() * InitNoiseRange - InitNoiseOffset,
      angle = random.nextDouble() * InitNoiseRange - InitNoiseOffset,
      angularVelocity = random.nextDouble() * InitNoiseRange - InitNoiseOffset
    )

  given NeuralNetworkEncoding[CartPoleState] with
    override def elements: Int = 4
    override def toSeq(elem: CartPoleState): Seq[Double] =
      Seq(elem.position, elem.velocity, elem.angle, elem.angularVelocity)

enum CartPoleAction(val value: Int) derives Enumerable:
  case Left extends CartPoleAction(0)
  case Right extends CartPoleAction(1)

object CartPoleAction:
  def force(action: CartPoleAction, forceMag: Double): Double =
    if action == Right then forceMag else -forceMag

case class StepResult(state: CartPoleState, reward: Double, done: Boolean)

case class CartPoleConfig(
    gravity: Double,
    massCart: Double,
    massPole: Double,
    length: Double,
    forceMag: Double,
    tau: Double,
    thetaThresholdRadians: Double,
    xThreshold: Double,
    maxSteps: Int
)

object CartPoleConfig:
  val Default: CartPoleConfig = CartPoleConfig(
    gravity = 9.8,
    massCart = 1.0,
    massPole = 0.1,
    length = 0.5,
    forceMag = 10.0,
    tau = 0.02,
    thetaThresholdRadians = math.toRadians(12.0),
    xThreshold = 2.4,
    maxSteps = 500
  )

private object CartPoleConstants:
  val UniformRodInertiaFactor = 4.0 / 3.0
  val InitNoiseRange = 0.1
  val InitNoiseOffset = 0.05
  val RewardOnStep = 1.0
  val RewardOnFailure = 0.0
