
package boids
import cs214.{Vector2, BoidSequence}
import ujson.Bool

def boidsWithinRadius(thisBoid: Boid, boids: BoidSequence, radius: Float): BoidSequence =
  boids.filter( (boid: Boid) => thisBoid.position.distanceTo(boid.position) <= radius && !thisBoid.equals(boid) )

/** Force pushing boids away from each other */
def avoidanceForce(thisBoid: Boid, boidsWithinAvoidanceRadius: BoidSequence): cs214.Vector2 =
  def avoid(boid: Boid): cs214.Vector2 =
    val distance = if !boid.position.equals(thisBoid.position) then
      boid.position.distanceTo( thisBoid.position ) else 1
    ( thisBoid.position - boid.position ).normalized./( distance )
  if boidsWithinAvoidanceRadius.isEmpty then cs214.Vector2.Zero else
    boidsWithinAvoidanceRadius.mapVector2( (boid: Boid) => avoid(boid) ).sum

// def avoidMigratoryForce(thisBoid: Boid, allBoids: BoidSequence): cs214.Vector2 =
//   if thisBoid.isInstanceOf[Migratory] then
//     cs214.Vector2.Zero
//   else
//     val migratoryBoids = allBoids.filter( boid =>
//       boid.isInstanceOf[Migratory] &&
//       thisBoid.position.distanceTo(boid.position) < 4f
//     )
//     if migratoryBoids.isEmpty then cs214.Vector2.Zero
//     else
//       migratoryBoids.mapVector2( migrator =>
//         val d = thisBoid.position.distanceTo(migrator.position)
//         val forceMagnitude = d / 3f
//         val direction = (thisBoid.position - migrator.position).normalized
//         direction * forceMagnitude
//       ).sum

/** Force pushing boids towards each other */
def cohesionForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  if boidsWithinPerceptionRadius.isEmpty then cs214.Vector2.Zero else
    boidsWithinPerceptionRadius
    .mapVector2( (boid: Boid) => boid.position )
    .sum
    ./(boidsWithinPerceptionRadius.length.floatValue())
    - thisBoid.position


/** Force pushing boids to align with the direction of their neighbors */
def alignmentForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  if boidsWithinPerceptionRadius.isEmpty then cs214.Vector2.Zero else
    boidsWithinPerceptionRadius
    .mapVector2( (boid: Boid) => boid.velocity )
    .sum
    ./(boidsWithinPerceptionRadius.length.floatValue())
    - thisBoid.velocity

/** Force keeping boids within simulation bounds */
def containmentForce(thisBoid: Boid, limits: BoundingBox): cs214.Vector2 =
  val up = if thisBoid.position.y < limits.ymin then cs214.Vector2.UnitDown else Vector2.Zero
  val down = if thisBoid.position.y > limits.ymax then cs214.Vector2.UnitUp else Vector2.Zero
  val left = if thisBoid.position.x < limits.xmin then cs214.Vector2.UnitRight else Vector2.Zero
  val right = if thisBoid.position.x > limits.xmax then cs214.Vector2.UnitLeft else Vector2.Zero
  up + down + left + right


/** Force pushing boids in a particular direction */
def migrationForce(thisBoid: Boid): Vector2 = 
  if thisBoid.isInstanceOf[Migratory] then
    thisBoid.asInstanceOf[Migratory].getDirection
  else Vector2.Zero

def totalForce(thisBoid: Boid, allBoids: BoidSequence, physics: Physics, tickCount: Int): Vector2 =
  val withinPerceptionRadius = boidsWithinRadius(thisBoid, allBoids, physics.perceptionRadius)
  val cohere = cohesionForce(thisBoid, withinPerceptionRadius)
  val align = alignmentForce(thisBoid, withinPerceptionRadius)
  val withinAvoidanceRadius = boidsWithinRadius(thisBoid, withinPerceptionRadius, physics.avoidanceRadius)
  val avoid = avoidanceForce(thisBoid, withinAvoidanceRadius)
  val contain = containmentForce(thisBoid, physics.limits)
  val migrate = migrationForce(thisBoid)
  // val avoidMigratory = avoidMigratoryForce(thisBoid, allBoids)
  val total =
    avoid * physics.avoidanceWeight +
    cohere * physics.cohesionWeight +
    align * physics.alignmentWeight +
    contain * physics.containmentWeight
  
  // Sets migration season to ticks 200 through 500
  if ((tickCount >= 200) && (tickCount <= 500))
    total + migrate * physics.migrationWeight // + avoidMigratory * physics.avoidMigratoryWeight 
  else
    total 


/** Returns the given boid, one tick later */
def tickBoid(thisBoid: Boid, allBoids: BoidSequence, physics: Physics, tickCount: Int): Boid =
  val acceleration = totalForce(thisBoid, allBoids, physics, tickCount)
  val newVelocity = thisBoid.velocity + acceleration
  val max = physics.maximumSpeed
  val min = physics.minimumSpeed
  val newBoundedVelocity = if newVelocity.norm > max then
    newVelocity.normalized * max
  else if newVelocity.norm < min then
    newVelocity.normalized * min
  else newVelocity
  val direction = 
    if thisBoid.isInstanceOf[Migratory] then
      thisBoid.asInstanceOf[Migratory].getDirection
    else Vector2.Zero
  Boid(
    position = thisBoid.position + thisBoid.velocity,
    velocity = newBoundedVelocity,
    thisBoid.size,
    thisBoid.color, 
    direction = direction)


/** Returns all the given boids, one tick later */
def tickWorld(allBoids: BoidSequence, physics: Physics, tickCount: Int): BoidSequence =
  allBoids.mapBoid( (boid: Boid) => tickBoid(boid, allBoids, physics, tickCount) )



