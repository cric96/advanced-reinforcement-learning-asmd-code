package it.unibo.gym

import java.awt.{Color, Graphics2D, Dimension, BasicStroke, Font}
import javax.swing.{JFrame, JPanel, WindowConstants}

case class WindowConfig(width: Int = 400, height: Int = 400, title: String = "CartPole")

case class TrackConfig(
    lineWidth: Int = 3,
    margin: Int = 20,
    tickSpacing: Int = 40,
    tickHeight: Int = 8,
    groundMarginBottom: Int = 60
)

case class CartConfig(
    width: Int = 60,
    height: Int = 30,
    cornerArc: Int = 8,
    bodyColor: Color = Color.DARK_GRAY,
    borderColor: Color = Color.BLACK,
    wheelRadius: Int = 10,
    wheelColor: Color = Color.BLACK,
    positionToPixelScale: Int = 60
)

case class PoleConfig(
    visualLength: Int = 150,
    lineWidth: Int = 4,
    color: Color = Color.RED,
    tipColor: Color = Color.RED.brighter(),
    tipRadius: Int = 8,
    tipDiameter: Int = 16
)

case class TextConfig(
    infoFont: Font = Font("SansSerif", Font.BOLD, 14),
    dangerFont: Font = Font("SansSerif", Font.BOLD, 16),
    textColor: Color = Color.BLACK,
    dangerColor: Color = Color.RED,
    marginX: Int = 15,
    lineOneY: Int = 25,
    lineTwoY: Int = 45,
    dangerLabel: String = "UNSTABLE",
    dangerLabelWidth: Int = 120,
    dangerAngleRadians: Double = math.toRadians(10),
    dangerPositionThreshold: Double = 2.0
)

case class CartPoleRendererConfig(
    window: WindowConfig = WindowConfig(),
    track: TrackConfig = TrackConfig(),
    cart: CartConfig = CartConfig(),
    pole: PoleConfig = PoleConfig(),
    text: TextConfig = TextConfig()
)

object CartPoleRendererConfig:
  val Default: CartPoleRendererConfig = CartPoleRendererConfig()

case class RenderState(
    episodeNumber: Int,
    episodeReward: Double,
    cartPoleState: CartPoleState
)

class CartPoleRenderer(config: CartPoleRendererConfig = CartPoleRendererConfig.Default) extends JPanel:
  import config.*

  private var renderState: RenderState = RenderState(0, 0.0, CartPoleState.zero)

  setPreferredSize(Dimension(window.width, window.height))
  setBackground(Color.WHITE)

  def update(newState: RenderState): Unit =
    renderState = newState
    repaint()

  override def paintComponent(g: java.awt.Graphics): Unit =
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

    val width = getWidth
    val height = getHeight
    val groundY = height - track.groundMarginBottom

    drawGround(g2, width, groundY)
    drawCartAndPole(g2, width, groundY, renderState.cartPoleState)
    drawInfoText(g2, width, renderState)

  private def drawGround(g2: Graphics2D, width: Int, groundY: Int): Unit =
    g2.setColor(Color.LIGHT_GRAY)
    g2.setStroke(BasicStroke(track.lineWidth))
    g2.drawLine(track.margin, groundY, width - track.margin, groundY)

    for x <- track.margin to (width - track.margin) by track.tickSpacing do
      g2.drawLine(x, groundY, x, groundY + track.tickHeight)

  private def drawCartAndPole(g2: Graphics2D, width: Int, groundY: Int, state: CartPoleState): Unit =
    val cartX = (width / 2 + state.position * cart.positionToPixelScale).toInt
    val cartY = groundY - cart.height

    g2.setColor(cart.bodyColor)
    g2.fillRoundRect(cartX - cart.width / 2, cartY, cart.width, cart.height, cart.cornerArc, cart.cornerArc)
    g2.setColor(cart.borderColor)
    g2.drawRoundRect(cartX - cart.width / 2, cartY, cart.width, cart.height, cart.cornerArc, cart.cornerArc)

    drawWheels(g2, cartX, groundY)
    drawPole(g2, cartX, cartY, state.angle)

  private def drawWheels(g2: Graphics2D, cartX: Int, groundY: Int): Unit =
    g2.setColor(cart.wheelColor)
    val wheelXOffsets = Seq(-cart.width / 2 - cart.wheelRadius / 2, cart.width / 2 - cart.wheelRadius / 2)
    wheelXOffsets.foreach { xOffset =>
      g2.fillOval(cartX + xOffset, groundY - cart.wheelRadius, cart.wheelRadius, cart.wheelRadius)
    }

  private def drawPole(g2: Graphics2D, cartX: Int, cartY: Int, angle: Double): Unit =
    val poleEndX = cartX + (math.sin(angle) * pole.visualLength).toInt
    val poleEndY = cartY - (math.cos(angle) * pole.visualLength).toInt

    g2.setColor(pole.color)
    g2.setStroke(BasicStroke(pole.lineWidth))
    g2.drawLine(cartX, cartY, poleEndX, poleEndY)

    g2.setColor(pole.tipColor)
    g2.fillOval(poleEndX - pole.tipRadius, poleEndY - pole.tipRadius, pole.tipDiameter, pole.tipDiameter)
    g2.setColor(pole.color)
    g2.drawOval(poleEndX - pole.tipRadius, poleEndY - pole.tipRadius, pole.tipDiameter, pole.tipDiameter)

  private def drawInfoText(g2: Graphics2D, width: Int, state: RenderState): Unit =
    g2.setColor(text.textColor)
    g2.setFont(text.infoFont)
    g2.drawString(s"Episode: ${state.episodeNumber}  Reward: ${state.episodeReward}", text.marginX, text.lineOneY)

    val angleDeg = math.round(math.toDegrees(state.cartPoleState.angle))
    g2.drawString(s"Angle: ${angleDeg}°  Position: ${f"${state.cartPoleState.position}%.2f"}", text.marginX, text.lineTwoY)

    if isUnstable(state.cartPoleState) then
      g2.setColor(text.dangerColor)
      g2.setFont(text.dangerFont)
      g2.drawString(text.dangerLabel, width - text.dangerLabelWidth, text.lineOneY)

  private def isUnstable(state: CartPoleState): Boolean =
    math.abs(state.angle) > text.dangerAngleRadians || math.abs(state.position) > text.dangerPositionThreshold

object CartPoleRenderer:
  def show(config: CartPoleRendererConfig = CartPoleRendererConfig.Default): CartPoleRenderer =
    val renderer = CartPoleRenderer(config)
    val frame = JFrame(config.window.title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(renderer)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    renderer
