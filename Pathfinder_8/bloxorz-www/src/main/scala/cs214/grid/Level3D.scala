package cs214
package grid

import Objects3D.*
import pathfinder.GameDef
import pathfinder.grid.StringParserTerrain

abstract class Level3D[State, Move] extends GameDef[State, Move] with StringParserTerrain:
  val level: String
  var state: State = startState

  def done: Boolean =
    isDone(state)

  def move(dir: Move): Unit =
    state = neighbors(state)
      .find(_._2 == dir)
      .map(_._1)
      .getOrElse(state)

  def reset(): Unit =
    state = startState

  def makeTerrain =
    terrainBlock(startPos.row, startPos.col).withTexture("wood") +:
      terrainBlock(goal.row, goal.col).withTexture("gold") +:
      vector.zipWithIndex
        .flatMap((v, i) => v.zipWithIndex.collect { case ('o', j) => (i, j) })
        .map((x, y) => terrainBlock(x, y).withTexture("stone"))

  def terrainBlock(x: Int, y: Int) =
    Object3D.Box.scale(Vec3D(1.0, 1.0, 0.2)).translate(Vec3D(x.toFloat, y.toFloat, -0.2))

  def makeBlock: Seq[Object3D]
  def render(renderer: Renderer): Unit
