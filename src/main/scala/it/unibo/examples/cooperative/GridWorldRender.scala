package it.unibo.examples.cooperative

import GridWorldRenderConfig.AgentPalette
import it.unibo.examples.cooperative.BoundedWorldEnvironment.State
import it.unibo.model.core.abstractions.Scheduler
import it.unibo.view.Render

import java.awt.{BasicStroke, Color, Dimension, Font, Graphics2D, RenderingHints}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

class GridWorldRender(using scheduler: Scheduler)(
    boundSize: Int,
    renderEvery: Int,
    frameDelayMs: Int,
    config: GridWorldRenderConfig = GridWorldRenderConfig.Default
) extends Render[State]:

  import config.*

  private val panel = GridPanel()
  private val frame = JFrame(window.title)

  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.add(panel)
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

  private var currentRenderInterval = renderEvery

  override def render(state: State): Unit =
    if scheduler.episode % currentRenderInterval == 0 then
      SwingUtilities.invokeLater: () =>
        panel.updateState(state)
        frame.setTitle(s"${window.title} -- Episode: ${scheduler.episode}")
      Thread.sleep(frameDelayMs)

  def renderEvery(value: Int): Unit = currentRenderInterval = value

  private class GridPanel extends JPanel:
    @volatile private var panelState: State = List.empty
    setPreferredSize(computePreferredSize)
    setBackground(cell.backgroundColor)

    override def paintComponent(g: java.awt.Graphics): Unit =
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      drawGrid(g2)
      drawAgents(g2, panelState)
      drawInfoOverlay(g2)

    private def computePreferredSize: Dimension =
      val gridWidth = boundSize * cell.size
      val gridHeight = boundSize * cell.size
      Dimension(gridWidth + window.padding * 2, gridHeight + window.padding * 2 + text.infoY + 10)

    private def drawGrid(g2: Graphics2D): Unit =
      g2.setColor(cell.gridLineColor)
      g2.setStroke(BasicStroke(cell.gridLineWidth))
      val offsetX = window.padding
      val offsetY = window.padding + text.infoY + 10

      for i <- 0 to boundSize do
        g2.drawLine(offsetX + i * cell.size, offsetY, offsetX + i * cell.size, offsetY + boundSize * cell.size)
        g2.drawLine(offsetX, offsetY + i * cell.size, offsetX + boundSize * cell.size, offsetY + i * cell.size)

    private def drawAgents(g2: Graphics2D, state: State): Unit =
      val offsetX = window.padding
      val offsetY = window.padding + text.infoY + 10

      state.zipWithIndex.foreach { case ((col, row), index) =>
        val centerX = offsetX + col * cell.size + cell.size / 2
        val centerY = offsetY + row * cell.size + cell.size / 2
        val color = AgentPalette(index % AgentPalette.size)

        g2.setColor(color)
        g2.fillOval(
          centerX - agent.circleRadius,
          centerY - agent.circleRadius,
          agent.circleRadius * 2,
          agent.circleRadius * 2
        )

        g2.setColor(agent.borderColor)
        g2.setStroke(BasicStroke(agent.borderWidth))
        g2.drawOval(
          centerX - agent.circleRadius,
          centerY - agent.circleRadius,
          agent.circleRadius * 2,
          agent.circleRadius * 2
        )
      }

    private def drawInfoOverlay(g2: Graphics2D): Unit =
      g2.setColor(text.textColor)
      g2.setFont(text.infoFont)
      val offsetX = window.padding
      val isAligned = checkAlignment(panelState)
      val statusText = if isAligned then "ALIGNED" else "Not aligned"
      val statusColor = if isAligned then text.alignedColor else text.textColor
      g2.drawString(s"Episode: ${scheduler.episode}  |  Agents: ${panelState.size}  |  Status: ", offsetX, text.titleY)
      g2.setColor(statusColor)
      g2.drawString(
        statusText,
        offsetX + g2.getFontMetrics.stringWidth(
          s"Episode: ${scheduler.episode}  |  Agents: ${panelState.size}  |  Status: "
        ),
        text.titleY
      )

    private def checkAlignment(state: State): Boolean =
      if state.isEmpty then false
      else
        val uniqueRows = state.distinctBy(_._2).size
        uniqueRows == 1

    def updateState(state: State): Unit =
      panelState = state
      repaint()

case class GridCellConfig(
    size: Int = 40,
    backgroundColor: Color = Color(240, 240, 240),
    gridLineColor: Color = Color(180, 180, 180),
    gridLineWidth: Float = 1.5f
)

case class AgentRenderConfig(
    circleRadius: Int = 18,
    borderColor: Color = Color.BLACK,
    borderWidth: Float = 2.0f
)

case class GridTextConfig(
    titleFont: Font = Font("SansSerif", Font.BOLD, 14),
    infoFont: Font = Font("SansSerif", Font.PLAIN, 12),
    textColor: Color = Color(50, 50, 50),
    alignedColor: Color = Color(0, 128, 0),
    margin: Int = 10,
    titleY: Int = 20,
    infoY: Int = 38
)

case class GridWindowConfig(
    title: String = "Grid World",
    padding: Int = 20
)

case class GridWorldRenderConfig(
    window: GridWindowConfig = GridWindowConfig(),
    cell: GridCellConfig = GridCellConfig(),
    agent: AgentRenderConfig = AgentRenderConfig(),
    text: GridTextConfig = GridTextConfig()
)

object GridWorldRenderConfig:
  val Default: GridWorldRenderConfig = GridWorldRenderConfig()

  val AgentPalette: Seq[Color] = Seq(
    Color(66, 133, 244),
    Color(234, 67, 53),
    Color(52, 168, 83),
    Color(251, 188, 4),
    Color(156, 39, 176),
    Color(0, 188, 212),
    Color(255, 87, 34),
    Color(121, 85, 72),
    Color(96, 125, 139),
    Color(233, 30, 99)
  )
