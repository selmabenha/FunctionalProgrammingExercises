package pathfinder
package grid

import TestLevels.*

class TerrainSuite extends munit.FunSuite:
  val rand = new scala.util.Random

  /** Tests that positions outside of the given terrain return false */
  def testTerrainLimits(terrain: Pos => Boolean, vector: Vector[Vector[Char]]) =
    val rowsCount = vector.length
    val columnsCount = vector(0).length

    // Last row + 1 (not in terrain)
    for x <- 0 until columnsCount + 1 do
      assertEquals(terrain(Pos(row = rowsCount, col = x)), false)

    // Last column + 1 (not in terrain)
    for y <- 0 until rowsCount + 1 do
      assertEquals(terrain(Pos(row = y, columnsCount)), false)

    // Negative
    for i <- 0 to 25 do
      val x = -(rand.nextInt(Integer.MAX_VALUE) + 1)
      val y = -(rand.nextInt(Integer.MAX_VALUE) + 1)
      assertEquals(terrain(Pos(row = y, col = x)), false)
      assertEquals(terrain(Pos(row = y, col = 0)), false)
      assertEquals(terrain(Pos(row = 0, col = x)), false)

    // Too far
    for i <- 0 to 25 do
      val x = (rand.between(columnsCount, Integer.MAX_VALUE))
      val y = (rand.between(rowsCount, Integer.MAX_VALUE))
      assertEquals(terrain(Pos(row = y, col = x)), false)
      assertEquals(terrain(Pos(row = y, col = 0)), false)
      assertEquals(terrain(Pos(row = 0, col = x)), false)

  test("terrainFunction: Level0 (3pts)"):
    val terrain = Level0.terrain
    val rowsCount = Level0.vector.length
    val columnsCount = Level0.vector(0).length

    // Test limits and outside of terrain
    testTerrainLimits(Level0.terrain, Level0.vector)

    // Row 0
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 0, col = x)), false)

    // Rows 1 to 3
    for y <- 1 to 3 do
      for x <- 0 until columnsCount do
        assertEquals(terrain(Pos(row = y, col = x)), 2 <= x && x <= 3)

    // Row 4
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 4, col = x)), false)

  test("terrainFunction: Level1 (3pts)"):
    val terrain = Level1.terrain
    val rowsCount = Level1.vector.length
    val columnsCount = Level1.vector(0).length

    // Test limits and outside of terrain
    testTerrainLimits(Level1.terrain, Level1.vector)

    // Rows 0 to 5
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 0, col = x)), x <= 2)
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 1, col = x)), x <= 5)
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 2, col = x)), x <= 8)
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 3, col = x)), 1 <= x && x < columnsCount)
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 4, col = x)), 5 <= x && x < columnsCount)
    for x <- 0 until columnsCount do
      assertEquals(terrain(Pos(row = 5, col = x)), 6 <= x && x <= 8)

  test("findChar: small terrain (3pts)"):
    val parser = new TestDef with StringParserTerrain:
      val level: String = ""

    val levelVect = Vector(
      Vector('a', 'b', 'c'),
      Vector('d', 'e', 'f'),
      Vector('g', 'h', 'i')
    )
    for x <- 0 until 3 do
      for y <- 0 until 3 do
        val c = ((y * 3 + x) + 97).toChar
        assertEquals(parser.findChar(c, levelVect), Pos(row = y, col = x))

  test("findChar: Level0 (3pts)"):
    assertEquals(Level0.findChar('S', Level0.vector), Pos(row = 1, col = 2))
    assertEquals(Level0.findChar('T', Level0.vector), Pos(row = 1, col = 3))

  test("findChar: Level1 (3pts)"):
    assertEquals(Level1.findChar('S', Level1.vector), Pos(row = 1, col = 1))
    assertEquals(Level1.findChar('T', Level1.vector), Pos(row = 4, col = 7))
