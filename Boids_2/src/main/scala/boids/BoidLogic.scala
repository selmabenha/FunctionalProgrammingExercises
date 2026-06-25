package boids
import cs214.{Vector2, BoidSequence}

def boidsWithinRadius(thisBoid: Boid, boids: BoidSequence, radius: Float): BoidSequence =
  boids.filter(x => x.position.distanceTo(thisBoid.position) < radius && x != thisBoid)

/** Force pushing boids away from each other */
def avoidanceForce(thisBoid: Boid, boidsWithinAvoidanceRadius: BoidSequence): cs214.Vector2 =
  boidsWithinAvoidanceRadius.foldLeftVector2(Vector2(0, 0)) {(sum, x) => 
    val d = thisBoid.position.distanceTo(x.position)
    if d == 0 then sum else sum + ((thisBoid.position - x.position).normalized * (1/d))
  }

/** Force pushing boids towards each other */
def cohesionForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  val B = boidsWithinPerceptionRadius.length
  if B == 0 then Vector2(0, 0) 
  else {
    val sumBoids = boidsWithinPerceptionRadius.foldLeftVector2(Vector2(0, 0)) {(sum, x) => sum + x.position}
    val centerOfMass = sumBoids / B.toFloat
    centerOfMass - thisBoid.position
  }

/** Force pushing boids to align with the direction of their neighbors */
def alignmentForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  val B = boidsWithinPerceptionRadius.length
  if B == 0 then Vector2(0, 0) 
  else {
    val sumBoids = boidsWithinPerceptionRadius.foldLeftVector2(Vector2(0, 0)) {(sum, x) => sum + x.velocity}
    val centerOfMass = sumBoids / B.toFloat
    centerOfMass - thisBoid.velocity
  }

/** Force keeping boids within simulation bounds */
def containmentForce(thisBoid: Boid, limits: BoundingBox): cs214.Vector2 =
  val Fx =
    if thisBoid.position.x > limits.xmax then  -1.0f
    else if thisBoid.position.x < limits.xmin then  1.0f
    else 0.0f

  val Fy = 
    if thisBoid.position.y < limits.ymin then 1.0f
    else if thisBoid.position.y > limits.ymax then -1.0f
    else 0.0f
  
  Vector2(Fx, Fy)

def totalForce(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Vector2 =
  val withinPerceptionRadius = boidsWithinRadius(thisBoid, allBoids, physics.perceptionRadius)
  val cohere = cohesionForce(thisBoid, withinPerceptionRadius)
  val align = alignmentForce(thisBoid, withinPerceptionRadius)
  val withinAvoidanceRadius = boidsWithinRadius(thisBoid, withinPerceptionRadius, physics.avoidanceRadius)
  val avoid = avoidanceForce(thisBoid, withinAvoidanceRadius)
  val contain = containmentForce(thisBoid, physics.limits)
  val total =
    avoid * physics.avoidanceWeight +
      cohere * physics.cohesionWeight +
      align * physics.alignmentWeight +
      contain * physics.containmentWeight
  total


/** Returns the given boid, one tick later */
def tickBoid(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Boid =
  val acceleration = totalForce(thisBoid, allBoids, physics)
  val velNormalized = (thisBoid.velocity + acceleration).normalized
  val velNorm = (thisBoid.velocity + acceleration).norm
  val velNormRes = if velNorm < physics.minimumSpeed then physics.minimumSpeed
                  else if velNorm > physics.maximumSpeed then physics.maximumSpeed
                  else velNorm

  Boid(thisBoid.position + thisBoid.velocity, velNormalized*velNormRes)

/** Returns all the given boids, one tick later */
def tickWorld(allBoids: BoidSequence, physics: Physics): BoidSequence =
  allBoids.mapBoid(x => tickBoid(x, allBoids, physics))
