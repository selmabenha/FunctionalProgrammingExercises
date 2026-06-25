package pathfinder
package grid

/** The case class `Pos` encodes positions in the terrain.
  *
  * IMPORTANT NOTES
  *   - The `row` coordinate denotes the vertical position on the grid
  *   - The `col` coordinate denotes the horizontal position on the grid
  *   - These coordinates increase when moving down or to the right
  *
  * Illustration:
  *
  * ```
  *     0 1 2 3   <- col axis
  *   0 o o o o
  *   1 o o o o
  *   2 o # o o    # is at position Pos(2, 1)
  *   3 o o o o
  *
  *   ^
  *   |
  *   row axis
  * ```
  */
case class Pos(row: Int, col: Int):
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)

/** Represents the definition of a game made of a 2D grid-like terrain */
trait TerrainGameDef[State, Move] extends GameDef[State, Move]:
  /** The terrain is represented as a function from positions to booleans. The
    * function returns `true` for every position that is inside the terrain.
    */
  type Terrain = Pos => Boolean

  /** The terrain of this game. This value is left abstract. */
  def terrain: Terrain

  /** Returns the list of states that can be obtained by moving the current
    * block, together with the corresponding move. The list includes the states
    * outside of the terrain.
    */
  def physicalNeighbors(state: State): Seq[(State, Move)]
