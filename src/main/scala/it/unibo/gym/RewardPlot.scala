package it.unibo.gym

import java.awt.Color
import javax.swing.{JFrame, WindowConstants}
import smile.plot.swing.{Canvas, Figure, Legend, Line, LinePlot, Point, ScatterPlot}

case class PlotStyleColors(
    scatterColor: Color = Color.BLUE,
    movingAverageColor: Color = Color.RED,
    thresholdColor: Color = Color.GREEN
)

case class PlotStyleLabels(
    scatterLegend: String = "Episode Reward",
    movingAverageLegend: String = "Moving Average",
    thresholdFormat: String = "Threshold (%.1f)"
)

case class PlotStyle(
    scatterPointChar: Char = '.',
    colors: PlotStyleColors = PlotStyleColors(),
    labels: PlotStyleLabels = PlotStyleLabels()
)

case class PlotAxisConfig(
    xLabel: String = "Episode",
    yLabel: String = "Reward",
    title: String = "Episode Reward Over Time"
)

case class PlotWindowConfig(
    title: String = "Training Rewards",
    size: (Int, Int) = (820, 520)
)

case class RewardPlotConfig(
    windowSize: Int = 20,
    threshold: Double = 200.0,
    upperBoundMargin: Double = 1.15,
    minUpperBound: Double = 1.0,
    axis: PlotAxisConfig = PlotAxisConfig(),
    window: PlotWindowConfig = PlotWindowConfig(),
    style: PlotStyle = PlotStyle()
)

object RewardPlotConfig:
  val Default: RewardPlotConfig = RewardPlotConfig()

class RewardPlot(config: RewardPlotConfig = RewardPlotConfig.Default):

  def show(rewards: List[Double]): Unit =
    val n = rewards.length
    val y = rewards.toArray
    val movingAvg = computeMovingAverage(rewards, config.windowSize)
    val upperBound = computeUpperBound(y, config.threshold, config.upperBoundMargin)

    val figure = new Figure(Array(0.0, 0.0), Array(math.max(n.toDouble, 1.0), upperBound))
    figure.setTitle(config.axis.title)
    figure.setAxisLabels(config.axis.xLabel, config.axis.yLabel)
    figure.add(createScatterPlot(y, n))
    figure.add(createMovingAveragePlot(movingAvg, config.windowSize))
    figure.add(createThresholdLine(n, config.threshold))
    figure.setLegendVisible(true)

    println(s"Max reward: ${yMax(y, config.threshold)}, Threshold: ${config.threshold}, Upper bound: $upperBound")

    val canvas = Canvas(figure)
    val frame = JFrame(config.window.title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(canvas)
    frame.setSize(config.window.size._1, config.window.size._2)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)

  private def computeMovingAverage(rewards: List[Double], window: Int): Array[Double] =
    rewards.sliding(window, 1).map(_.sum / window).toArray

  private def computeUpperBound(y: Array[Double], threshold: Double, margin: Double): Double =
    math.max(threshold * margin, math.max(yMax(y, threshold), config.minUpperBound))

  private def yMax(y: Array[Double], threshold: Double): Double =
    if y.nonEmpty then y.max else threshold

  private def createScatterPlot(y: Array[Double], n: Int): ScatterPlot =
    val points = new Point(
      (0 until n).map(i => Array(i.toDouble, y(i))).toArray,
      config.style.scatterPointChar,
      config.style.colors.scatterColor
    )
    new ScatterPlot(
      Array(points),
      Array(new Legend(config.style.labels.scatterLegend, config.style.colors.scatterColor))
    )

  private def createMovingAveragePlot(movingAvg: Array[Double], window: Int): LinePlot =
    LinePlot.of(
      movingAvg.indices.map(i => Array(i.toDouble + window - 1, movingAvg(i))).toArray,
      Line.Style.SOLID,
      config.style.colors.movingAverageColor,
      config.style.labels.movingAverageLegend
    )

  private def createThresholdLine(n: Int, threshold: Double): LinePlot =
    LinePlot.of(
      Array(Array(0.0, threshold), Array(n.toDouble, threshold)),
      Line.Style.DASH,
      config.style.colors.thresholdColor,
      f"${config.style.labels.thresholdFormat}".format(threshold)
    )

object RewardPlot:
  def apply(config: RewardPlotConfig = RewardPlotConfig.Default): RewardPlot =
    new RewardPlot(config)
