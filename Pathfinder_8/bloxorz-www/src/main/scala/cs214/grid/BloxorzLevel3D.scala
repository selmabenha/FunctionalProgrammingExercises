package cs214
package grid

import Objects3D.*
import pathfinder.grid.*

class BloxorzLevel3D(val level: String) extends Level3D[BloxorzState, BloxorzMove] with BloxorzLevel:

  def render(renderer: Renderer) =
    val block = this.state.asInstanceOf[BloxorzState]
    val objects = makeTerrain ++ makeBlock
    val pov = Vec3D(Math.min(block.b1.row, block.b2.row).toFloat, Math.min(block.b1.col, block.b2.col).toFloat, 0.0)
    renderer.drawScene(objects, pov)

  def makeBlock =
    val block = this.state.asInstanceOf[BloxorzState]
    Object3D.Box
      .translate(Vec3D(block.b1.row.toFloat, block.b1.col.toFloat, 0.0)).withTexture("diamond") +:
      Object3D.Box
        .translate(
          Vec3D(
            block.b2.row.toFloat,
            block.b2.col.toFloat,
            if block.b1 == block.b2 then 1.0 else 0.0
          )
        ).withTexture("diamond") +: Seq.empty
