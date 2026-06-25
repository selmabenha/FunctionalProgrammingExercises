package pathfinder
package grid

/** A simple Bloxorz level */
type BloxorzLevel = BloxorzDef

/** A main object that can be used to execute the Bloxorz solver */
object BloxorzLevels:

  /** A level constructed using the `InfiniteTerrain` trait which defines the
    * terrain to be valid at every position.
    */
  object InfiniteLevel extends BloxorzLevel with InfiniteTerrain:
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)

  object Level0 extends BloxorzLevel with StringParserTerrain:
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

  /** Level 1 of the official Bloxorz game */
  object Level1 extends BloxorzLevel with StringParserTerrain:
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

  object Level2 extends BloxorzLevel with StringParserTerrain:
    val level =
      """---o----
        |---oo---
        |oooo-ooo
        |oToooo-S
        |ooo-oooo
        |ooo--ooo""".stripMargin
