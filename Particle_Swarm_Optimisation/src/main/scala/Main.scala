import javafx.application.Application
import javafx.stage.Stage
import java.util
import javafx.scene.control.ChoiceDialog
import javafx.scene.canvas.Canvas
import javafx.scene.chart.XYChart.Series
import javafx.scene.chart.{BarChart, CategoryAxis, NumberAxis, XYChart}
import javafx.scene.layout.GridPane
import javafx.scene.Scene
import javafx.animation.AnimationTimer
import javafx.scene.paint.Color

class Main extends Application {

  override def start(primaryStage: Stage) {

    // Canvas initialization.
    // Particles traverse across the canvas.
    val canvas:Canvas = new Canvas(640, 580)

    // Initialize choices of numberOfParticles for the ChoiceDialog.
    val choices = new util.ArrayList[Int]
    choices.add(100)
    choices.add(200)
    choices.add(300)
    choices.add(400)
    choices.add(500)
    choices.add(600)
    choices.add(700)
    choices.add(800)
    choices.add(900)
    choices.add(1000)

    // ChoiceDialog initialization.
    val dialog = new ChoiceDialog[Int](200, choices)
    dialog.setTitle("PSO written by C. Caburian")
    dialog.setHeaderText("PSO Configuration")
    dialog.setContentText("Choose how many particles to run: ")
    val result = dialog.showAndWait
    if (result.isPresent) {

      // Get the user-selected option to set the numberOfParticles for
      // the initialSwarmState.
      val numberOfParticles = result.get

      // Mutable variable that holds the SwarmState object for
      // each AnimationTimer tick.
      var currentSwarmState:Particles.SwarmState =
        Particles.initialSwarmState(numberOfParticles)

      // Axes for charting each particle's altitude coordinates.
      val altitudeCatAxis = new CategoryAxis()
      altitudeCatAxis.setAnimated(false)
      altitudeCatAxis.setTickLabelsVisible(false)

      // Auto-ranging NumberAxis is chosen so that the altitudeAxis provides
      // useful feedback for both very large and very small altitude values
      // (a fixed range would only favour one over the other).
      val altitudeAxis = new NumberAxis()
      altitudeAxis.setAnimated(false)

      // BarCharts for altitude coordinates.
      val altitudeChart = new BarChart(altitudeCatAxis, altitudeAxis)
      altitudeChart.setAnimated(false)
      altitudeChart.setLegendVisible(false)
      altitudeChart.setTitle("Particle Altitudes")

      // Series to hold successive altitude values.
      val altitudeSeries = new Series[String, Number]()

      // Chart and series initialization.
      for {
        (particle, index) <- currentSwarmState.collectParticleSwarm.zipWithIndex
      } altitudeSeries.getData.add(new XYChart.Data(index.toString, particle.altitude))

      // Add data series to the altitudeChart.
      altitudeChart.getData.add(altitudeSeries)

      // UI layout.
      val pane = new GridPane()
      pane.add(canvas, 0, 0)
      pane.add(altitudeChart, 0, 1)

      // UI Scene and stage initialization.
      val scene = new Scene(pane)
      primaryStage.setScene(scene)
      primaryStage.show()

      // AnimationTimer to progress the PSO.
      new AnimationTimer() {
        override def handle(now: Long): Unit = {

          // Updates the currentSwarmState with a new (updated) SwarmState object.
          currentSwarmState = currentSwarmState.updateSwarmState()

          // Update mutable variables
          Particles.updateCurrentTopFive(currentSwarmState)
          Particles.updateGlobalHigh()

          // Draw the Particles and GlobalHighMarking objects for the
          // currentSwarmState on the canvas.
          renderPSO(currentSwarmState, canvas)

          // Log to standard out at each step of the loop:
          // - "Best Location" seen so far, globally.
          // - "Best Altitude" derived from the Best Location.
          // - "Five Highest Particles" in the currentSwarmState,
          //      each particle's coordinates are given as "((x, y), altitude)".
          println(
            "Best Location:" + Particles.globalHigh._1
            + " | Best Altitude: " + Particles.globalHigh._2
            + " | Five Highest Particles: " + Particles.currentTopFive
          )

          // Get currentSwarmState's altitude values for the altitudeChart.
          for {
            (particle, index) <- currentSwarmState.collectParticleSwarm.zipWithIndex
          } altitudeSeries.getData.get(index).setYValue(particle.altitude)
        }
      }.start()
    }
  }

  // Render the swarmState on the canvas.
  def renderPSO(swarmState:Particles.SwarmState, canvas:Canvas):Unit = {

    // Canvas color.
    val g2d = canvas.getGraphicsContext2D
    g2d.setFill(Color.BLACK)
    g2d.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

    // SwarmItems' color/size.
    swarmState.swarmItems foreach {
      case Particles.Particle(position,_,_,_) =>
        g2d.setFill(Color.WHITE)
        g2d.fillOval(position._1, position._2, 4, 4)
      case Particles.GlobalHighMarking(position) =>
        g2d.setFill(Color.RED)
        g2d.fillOval(position._1, position._2, 10, 10)
    }
  }
}

object Main {
  def main(args:Array[String]):Unit = {
    Application.launch(classOf[Main], args:_*)
  }
}