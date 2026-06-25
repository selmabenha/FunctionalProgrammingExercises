package pathfinder
package grid

/** In Bloxorz, we can move left, right, up or down. */
enum BloxorzMove:
  case Left, Right, Up, Down

/** In Bloxorz, a state is represented by the position of the two cubes that it
  * consists of. We make sure that `b1` is lexicographically smaller than `b2`.
  */
case class BloxorzState(b1: Pos, b2: Pos):
  // checks the requirement mentioned above
  require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

  /** Returns a block where the `row` coordinates of `b1` and `b2` are changed
    * by `d1` and `d2`, respectively.
    */
  private def deltaRow(d1: Int, d2: Int) = BloxorzState(b1.deltaRow(d1), b2.deltaRow(d2))

  /** Returns a block where the `col` coordinates of `b1` and `b2` are changed
    * by `d1` and `d2`, respectively.
    */
  private def deltaCol(d1: Int, d2: Int) = BloxorzState(b1.deltaCol(d1), b2.deltaCol(d2))

  def left =
    if isStanding then deltaCol(-2, -1)
    else if b1.row == b2.row then deltaCol(-1, -2)
    else deltaCol(-1, -1)

  def right =
    if isStanding then deltaCol(1, 2)
    else if b1.row == b2.row then deltaCol(2, 1)
    else deltaCol(1, 1)

  def up =
    if isStanding then deltaRow(-2, -1)
    else if b1.row == b2.row then deltaRow(-1, -1)
    else deltaRow(-1, -2)

  def down =
    if isStanding then deltaRow(1, 2)
    else if b1.row == b2.row then deltaRow(1, 1)
    else deltaRow(2, 1)

  /** Returns `true` if the block is standing. */
  def isStanding: Boolean = b1 == b2

/** Bloxorz game definition */
trait BloxorzDef extends TerrainGameDef[BloxorzState, BloxorzMove]:

  /** The target position where the state has to go. This value is left
    * abstract.
    */
  def goal: Pos

  /** The position where the state is located initially.
    *
    * This value is left abstract, it will be defined in concrete instances of
    * the game.
    */
  def startPos: Pos

  def startState: BloxorzState =
    ???

  def physicalNeighbors(state: BloxorzState): Seq[(BloxorzState, BloxorzMove)] =
    ???

  /** Returns `true` if the state is entirely inside the terrain. */
  def isLegal(state: BloxorzState): Boolean =
    ???

  def neighbors(state: BloxorzState): Seq[(BloxorzState, BloxorzMove)] =
    ???

  def isDone(state: BloxorzState): Boolean =
    ???
