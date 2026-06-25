package boids

import cs214.{BoidSequence, Vector2}
import java.awt.Color


object Boids:
  val DEFAULT_REGULAR_SIZE: Float = 1f
  val DEFAULT_REGULAR_COLOR: Color = Color.BLUE
  val DEFAULT_BIRD_SIZE: Float = 1.2f
  val DEFAULT_BIRD_COLOR: Color = Color.RED

case class Boid(
    position: Vector2,
    velocity: Vector2,
    size: Float,
    color: Color,
) 

class Migratory(
    position: Vector2,
    velocity: Vector2,
    size: Float = Boids.DEFAULT_BIRD_SIZE,
    color: Color = Boids.DEFAULT_BIRD_COLOR,
    direction: Vector2,


) extends Boid(position, velocity, size, color){
  def getDirection: Vector2 = direction
}

//Companion object to create Migraotry Boids using Boid key word
object Boid:
  def apply(position: Vector2, velocity: Vector2, size: Float = Boids.DEFAULT_REGULAR_SIZE, color: Color = Boids.DEFAULT_REGULAR_COLOR, direction: Vector2 =Vector2(0,0)): Boid = 
    //A boid with no migratory direction is just a regular Boid
    if direction == Vector2(0,0) then
      Boid(position, velocity, size, color)
    else
      Migratory(position, velocity, size, color, direction)
