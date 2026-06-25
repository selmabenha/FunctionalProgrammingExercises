package pathfinder.grid

object TestLevels:
  object Level0 extends TestDef with StringParserTerrain:
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

  object Level1 extends TestDef with StringParserTerrain:
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

  object Level2 extends TestDef with StringParserTerrain:
    val level =
      """---o----
        |---oo---
        |oooo-ooo
        |oToooo-S
        |ooo-oooo
        |ooo--ooo""".stripMargin

  object Level3 extends TestDef with StringParserTerrain:
    val level =
      """oo-o
        |-oS-
        |oo-o
        |-oT-""".stripMargin
