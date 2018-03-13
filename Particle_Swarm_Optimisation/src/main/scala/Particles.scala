object Particles {

  // Types.
  type Position = (Double, Double)
  type Velocity = (Double, Double)

  // XYAltitude is used to express a position coordinate with its
  // altitude coordinate that will be calculated using height(x, y).
  type XYAltitude = (Position, Double)

  /*returnHighest
  * A utility method that takes two XYAltitude types and returns
  * whichever has the highest altitude coordinate.
  * */
  def returnHighest(XYAltitudeA:XYAltitude, XYAltitudeB:XYAltitude):XYAltitude = {
    val altitudeA = XYAltitudeA._2
    val altitudeB = XYAltitudeB._2
    if (altitudeA > altitudeB) XYAltitudeA else XYAltitudeB
  }

  /*height
  * The height function given in the Assignment.
  * Takes an (x, y) coordinate and outputs an altitude.
  * */
  def height(x:Double, y:Double):Double =
    Math.sin(10*Math.PI*x)*Math.sin(10*Math.PI*y) - 10*(Math.pow(x-0.5,2) + Math.pow(y-0.5,2))

  // Values for initialization based on the canvasCentre.
  val canvasCentre:Position = (640 / 2.0, 580 / 2.0)
  val initialXYAltitude:XYAltitude = (canvasCentre, height(canvasCentre._1, canvasCentre._2))

  // Mutable variables to store the highest XYAltitude (globalHigh) and
  // the XYAltitude coordinates of top five highest particles in a SwarmState.
  var currentTopFive:Seq[XYAltitude] = Seq(initialXYAltitude)
  var globalHigh:XYAltitude = initialXYAltitude

  /*updateCurrentTopFive
  * Updates the variable currentTopFive with the five highest particles
  * in the given SwarmState object.
  * */
  def updateCurrentTopFive(swarmState:SwarmState):Unit = {

    // Entire swarm is sorted by altitude from highest first to lowest.
    val sortedSwarm =
      swarmState.collectParticleSwarm.sortBy(particle => particle.altitude).reverse

    // Five highest particles for each AnimationTimer tick are derived
    // from the top five particles in sortedSwarm.
    currentTopFive =
      sortedSwarm take 5  map { particle => (particle.position, particle.altitude) }
  }

  /*updateGlobalHigh
  * Updates the variable globalHigh with the XYAltitude of the highest
  * Particle in the current swarmState if it surpasses the current
  * altitude of globalHigh.
  * */
  def updateGlobalHigh():Unit =
    globalHigh = returnHighest(globalHigh, currentTopFive.head)


  /*SwarmItem
  * A sequence of SwarmItems defines a SwarmState.
  * A SwarmItem is either a Particle or a GlobalHighMarking.
  * */
  trait SwarmItem

  /*Particle
  * A Particle object has a position, velocity, altitude, and highestXYAlt.
  * Each particle object stores the highest Altitude it has seen so far in
  * highestXYAlt.
  * */
  case class Particle(position: Position,
                      velocity: Velocity,
                      altitude:Double,
                      highestXYAlt:XYAltitude) extends SwarmItem {

    /*updateVelocity
    * For each swarmState each particle's velocity is updated
    * based on a given equation that takes into account the
    * highest point it has seen, and the highest point the
    * whole system has seen so far.
    * */
    def updateVelocity(position:Position,
                       velocity:Velocity,
                       highestXYAlt:XYAltitude):Velocity = {

      // Terms used for the given equation (assigned for readability).
      val x = position._1
      val y = position._2
      val vx = velocity._1
      val vy = velocity._2
      val r1 = Math.random
      val r2 = Math.random
      val r3 = Math.random
      val r4 = Math.random
      val lx = highestXYAlt._1._1
      val ly = highestXYAlt._1._2
      val gx = globalHigh._1._1
      val gy = globalHigh._1._2

      // Equations given in the assignment.
      val updatedVx = (0.5*vx) + 2*r1*(lx-x) + 2*r2*(gx-x)
      val updatedVy = (0.5*vy) + 2*r3*(ly-y) + 2*r4*(gy-y)

      // The resulting velocity values are clamped to the range -1 to 1
      val clampedVx = -1.0 max updatedVx min 1.0
      val clampedVy = -1.0 max updatedVy min 1.0

      (clampedVx, clampedVy)
    }

    /*updatePosition
    * Each particle's position is updated based on a given equation
    * that takes into account its velocity.
    * */
    def updatePosition(position:Position, velocity:Velocity):Position = {

      // Values used for the given equation (assigned for readability).
      val x = position._1
      val y = position._2
      val vx = velocity._1
      val vy = velocity._2

      // Equations given in the assignment.
      val updatedX = x + vx
      val updatedY = y + vy

      (updatedX, updatedY)
    }

    /*updateParticle
    * Returns a new Particle object with updated fields.
    * */
    def updateParticle():Particle = {

      // Calculate updated fields.
      val newPosition = updatePosition(position, velocity)
      val newAltitude = height(newPosition._1, newPosition._2)
      val newHighestXYAlt = returnHighest(highestXYAlt, (newPosition, newAltitude))
      val newVelocity = updateVelocity(newPosition, velocity, newHighestXYAlt)
                        // The choice of these parameters are discussed in README.txt

      // Updated fields passed into a new Particle object.
      Particle(newPosition, newVelocity, newAltitude, newHighestXYAlt)
    }
  }

  /*GlobalHighMarking
  * A GlobalHighMarking object copies the current globalHigh's
  * coordinate into its position, thereby marking the highest
  * Altitude seen globally so far (colored in red on the canvas).
  * It is allowed to jump around or smoothly traverse the terrain.
  * */
  case class GlobalHighMarking(position:Position) extends SwarmItem {

    // Update the position of the marking using the
    // position coordinate of globalHigh.
    def updateGlobalHighMarking():GlobalHighMarking =
      GlobalHighMarking(globalHigh._1)
  }

  /*SwarmState
  * A SwarmState object is a sequence of SwarmItems to
  * be updated at each tick of the AnimationTimer.
  * */
  case class SwarmState(swarmItems:Seq[SwarmItem]) {

    // Collect methods for extracting specific classes of SwarmItems.
    def collectParticleSwarm:Seq[Particles.Particle] =
      swarmItems collect { case p:Particle => p }
    def collectGlobalHighMarking:Seq[Particles.GlobalHighMarking] =
      swarmItems collect { case g:GlobalHighMarking => g }

    /*updateSwarmState
    * Returns a new SwarmState object by calling each
    * swarmItem's corresponding update method.*/
    def updateSwarmState():SwarmState = {
      SwarmState(
        swarmItems map {
          case p:Particle => p.updateParticle()
          case g:GlobalHighMarking => g.updateGlobalHighMarking()
        }
      )
    }
  }

  /*initialSwarmState
  * Initializes the PSO by populating the canvas with a
  * GlobalHighMarking object and a randomizedParticleSwarm.
  * */
  def initialSwarmState(numberOfParticles:Int):SwarmState = {

    /*randomParticle
    * Generates a random particle used to fill the initial swarmState and
    * populate the canvas.
    * A particle's initial position is randomized and limited within a circle
    * centred on the canvasCentre and a radius equal to the vertical length
    * of the canvas.
    * */
    def randomParticle():Particle = {

      // Converts a polar co-ordinate to a Cartesian vector.
      def vector(r:Double, theta:Double):(Double, Double) =
        (r * Math.cos(theta), r * Math.sin(theta))

      // Randomized polar coordinate values.
      val maxRadius = canvasCentre._2
      val radius = Math.random * maxRadius
      val theta = Math.random * 2 * Math.PI

      // Randomized coordinates are converted to
      // Cartesian coordinates for canvas placement.
      val position =
        (vector(radius, theta)._1 + canvasCentre._1,  // x-coordinate
          vector(radius, theta)._2 + canvasCentre._2) // y-coordinate
      val altitude = height(position._1, position._2)
      val xyAltitude = (position, altitude)

      // Returns generated particle with zero velocity and a
      // randomized position (affecting the altitude and xyAltitude).
      Particle(position,(0,0), altitude, xyAltitude)
    }

    // Generates a sequence of randomParticles with a user-selected numberOfParticles.
    val randomizedParticleSwarm = 0 until numberOfParticles map { _ => randomParticle() }

    // Returns an initial SwarmState object consisting of a GlobalHighMarking
    // object generated using the initialHighest Coordinate and a sequence of
    // randomParticles.
    SwarmState(randomizedParticleSwarm :+ GlobalHighMarking(initialXYAltitude._1))
  }
}