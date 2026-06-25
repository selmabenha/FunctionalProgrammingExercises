# Software Construction Unguided Callback team142

## Aim
We plan to add migration birds to the Boids lab. We would like to create a special type of boid that migrates during certain seasons, so only part of the time, during which other regular boids will be more avoidant of the migration birds than during non-migration seasons. We will be implementing classes, recursion functions, API functions (map, fold, etc.), and polymorphism. Selma will be creating the seasons via the tick world, Romeo will create the migration forces, Flavia will create an avoidance force between migration and regular boids during the migration season, and Océane will create the class of separate migration boids.

## Description
## Feature Explanation: avoidMigratorsForce

In the following section, we will detail more precisely how our new features work.

### AvoidMigratorsForce

During the migration season, Boids will try to avoid Migrators. Migrators that are within a radius of less than 4 units will exert a force on Regular Boids. This force is equal to the distance between the Migrators and the Regular Boids, divided by three.

In mathematical terms:

Let \( d \) be the distance between a Migrator and a Regular Boid, with \( d < 4 \).

The avoidance force 
```math
 F_{\text{avoidmigratoryForce}}
 ```

 is then defined as:

```math
F_{\text{avoidMigratoryForce}} = \frac{d}{3}
```

where:
- \( d \) is the distance between the Regular Boid and the Migrator.

Thus, each Migrator within a radius of 4 around the Regular Boid will exert a force proportional to 
```math
 \frac{d}{3} 
 ```
