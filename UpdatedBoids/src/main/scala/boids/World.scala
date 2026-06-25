package boids

import cs214.{BoidNil, BoidCons, BoidSequence, Vector2}
import scala.util.Random
import java.awt.Color

class World(val physics: Physics, val tickCount: Int = 0):
  // Create immutable tickCount that updates on each tick by updating World instance on each tick
  def tick(boids: BoidSequence): (World, BoidSequence) = 
    
    // Pass tickCount as argument to use as a condition on the migration season
    val newBoids = tickWorld(boids, physics, tickCount)

    // Create new World instance with increasing ticks, reset to 0 after 800 ticks (imitation of the seasons)
    if(tickCount == 800) (new World(physics, 0), newBoids)
    else (new World(physics, tickCount + 1), newBoids)
    

object World:
  def apply(physics: Physics): World = new World(physics, 0)
  enum CardinalDirection(x: Float, y: Float) extends Vector2(x,y):
    case North extends CardinalDirection(0,-1)
    case Northeast extends CardinalDirection(1,-1)
    case East extends CardinalDirection(1,0)
    case Southeast extends CardinalDirection(1,1)
    case South extends CardinalDirection(0,1)
    case Southwest extends CardinalDirection(-1,1)
    case West extends CardinalDirection(-1,0)
    case Northwest extends CardinalDirection(-1,-1)

  def createRandomBoid(physics: Physics): Boid =
    val x = Random.between(physics.limits.xmin.toFloat, physics.limits.xmax.toFloat)
    val y = Random.between(physics.limits.ymin.toFloat, physics.limits.ymax.toFloat)
    val initialPosition = Vector2(x, y)

    val initialRotation = Random.between(0f, 2 * Math.PI.toFloat)
    val initialSpeed = Random.between(physics.minimumSpeed, physics.maximumSpeed)
    val initialVelocity = Vector2.UnitUp.rotate(initialRotation) * initialSpeed

    Boid(initialPosition, initialVelocity)

  def createRandom(numBoids: Int, physics: Physics): BoidSequence =
    (0 until numBoids).foldLeft[BoidSequence](BoidNil()) { (seq, _) =>
      BoidCons(createRandomBoid(physics), seq)
    }

case class BoundingBox(
    xmin: Int,
    xmax: Int,
    ymin: Int,
    ymax: Int
)

case class Physics(
    limits: BoundingBox,

    minimumSpeed: Float,
    maximumSpeed: Float,
    perceptionRadius: Float,
    avoidanceRadius: Float,
    avoidanceWeight: Float,
    cohesionWeight: Float,
    alignmentWeight: Float,
    containmentWeight: Float,
    migrationWeight: Float = 0.25f
   // avoidMigratoryWeight: Float = 0.001f
)

object Physics:
  val defaultLimits = BoundingBox(
    xmin = -500,
    xmax = 500,
    ymin = -350,
    ymax = 350
  )
  val default = Physics(
    limits = defaultLimits,
    minimumSpeed = 1f,
    maximumSpeed = 8f,
    perceptionRadius = 80f,
    avoidanceRadius = 15f,
    avoidanceWeight = 1f,
    cohesionWeight = 0.001f,
    alignmentWeight = 0.027f,
    containmentWeight = 0.5f
  )
