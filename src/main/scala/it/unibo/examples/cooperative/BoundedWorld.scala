package it.unibo.examples.cooperative

import it.unibo.model.core.abstractions.{AI, Enumerable, MultiAgentEnvironment}
import it.unibo.model.core.learning.Learner

import scala.util.Random

class BoundedWorldEnvironment(using random: Random)(
    agents: Int,
    boundSize: Int
) extends MultiAgentEnvironment[BoundedWorldEnvironment.State, BoundedWorldEnvironment.Action]:
  import BoundedWorldEnvironment.*

  var state: State = generatePosition

  def act(actions: Seq[Action]): Seq[Double] =
    state = state.zip(actions).map { case (pos, action) => action.updatePosition(pos, boundSize) }
    val alignmentReward = computeAlignmentReward
    state.map(_ => alignmentReward)

  override def reset(): Unit = state = generatePosition

  private def computeAlignmentReward: Double =
    val uniqueRows = state.distinctBy(_._2).size
    if uniqueRows == 1 then 0.0 else -1.0

  private def generatePosition: State =
    val half = boundSize / 2
    (0 until agents).map { i =>
      val row = (i % 2) * half
      val col = random.nextInt(boundSize)
      (col, row + random.nextInt(half))
    }.toList

enum MovementAction derives Enumerable:
  case Up, Down, Right, Left, NoOp
  def updatePosition(position: (Int, Int), bound: Int): (Int, Int) =
    def pacmanEffect(coordinate: Int): Int = if coordinate < 0 then bound - 1 else coordinate
    val (x, y) = position
    this match
      case Up => (x, (y + 1) % bound)
      case Down => (x, pacmanEffect(y - 1))
      case Left => (pacmanEffect(x - 1), y)
      case Right => ((x + 1) % bound, y)
      case NoOp => position

object BoundedWorldEnvironment:
  type State = List[(Int, Int)]
  type Action = MovementAction

  case class RelativeState(rowDiff: Int, colDiff: Int)

  def toRelative(state: State, agentIndex: Int, boundSize: Int): RelativeState =
    val (myRow, myCol) = state(agentIndex)
    val otherIndex = if agentIndex == 0 then 1 else 0
    val (otherRow, otherCol) = state(otherIndex)
    val rowDiff = ((otherRow - myRow) % boundSize + boundSize) % boundSize
    val colDiff = ((otherCol - myCol) % boundSize + boundSize) % boundSize
    RelativeState(rowDiff, colDiff)

  def fromRelative(rs: RelativeState, myPos: (Int, Int), boundSize: Int): (Int, Int) =
    ((myPos._1 + rs.rowDiff) % boundSize, (myPos._2 + rs.colDiff) % boundSize)

  class RelativeStateAgent(
      qAgent: AI.Agent[RelativeState, Action] & Learner[RelativeState, Action],
      agentIndex: Int,
      boundSize: Int
  ) extends AI.Agent[State, Action], Learner[State, Action]:
    override def optimal: State => Action = s => qAgent.optimal(toRelative(s, agentIndex, boundSize))
    override def behavioural: State => Action = s => qAgent.behavioural(toRelative(s, agentIndex, boundSize))
    override def improve(state: State, action: Action, reward: Double, nextState: State, done: Boolean): Unit =
      qAgent.improve(toRelative(state, agentIndex, boundSize), action, reward, toRelative(nextState, agentIndex, boundSize), done)
    override def reset(): Unit = qAgent.reset()
