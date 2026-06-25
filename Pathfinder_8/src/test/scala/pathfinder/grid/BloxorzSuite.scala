package pathfinder
package grid

import BloxorzMove.*
import BloxorzLevels.*

class BloxorzSuite extends munit.FunSuite:

  test("startState: InfiniteLevel (1pts)"):
    assertEquals(InfiniteLevel.startState, BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 3)))

  test("startState: Level0 (1pts)"):
    assertEquals(Level0.startState, BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 2)))

  test("startState: Level1 (1pts)"):
    assertEquals(Level1.startState, BloxorzState(Pos(row = 1, col = 1), Pos(row = 1, col = 1)))

  test("startState: Level2 (1pts)"):
    assertEquals(Level2.startState, BloxorzState(Pos(row = 3, col = 7), Pos(row = 3, col = 7)))

  def sortNeighbors(entry: (BloxorzState, BloxorzMove)) = entry._2.ordinal

  test("physicalNeighbors: InfiniteLevel, start state (2pts)"):
    val state =
      BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 3)) // Not using startState to test only physicalNeighbors
    val result = InfiniteLevel.physicalNeighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 1, col = 1), Pos(row = 1, col = 2)), Left),
      (BloxorzState(Pos(row = 1, col = 4), Pos(row = 1, col = 5)), Right),
      (BloxorzState(Pos(-1, 3), Pos(row = 0, col = 3)), Up),
      (BloxorzState(Pos(row = 2, col = 3), Pos(row = 3, col = 3)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("physicalNeighbors: Level0, start state (2pts)"):
    val state =
      BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 2)) // Not using startState to test only physicalNeighbors
    val result = Level0.physicalNeighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 1, col = 0), Pos(row = 1, col = 1)), Left),
      (BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 4)), Right),
      (BloxorzState(Pos(-1, 2), Pos(row = 0, col = 2)), Up),
      (BloxorzState(Pos(row = 2, col = 2), Pos(row = 3, col = 2)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("physicalNeighbors: Level1, start state (2pts)"):
    val state = BloxorzState(Pos(row = 1, col = 1), Pos(row = 1, col = 1))
    val result = Level1.physicalNeighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(1, -1), Pos(row = 1, col = 0)), Left),
      (BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 3)), Right),
      (BloxorzState(Pos(-1, 1), Pos(row = 0, col = 1)), Up),
      (BloxorzState(Pos(row = 2, col = 1), Pos(row = 3, col = 1)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("physicalNeighbors: Level2, start state (2pts)"):
    val state = BloxorzState(Pos(row = 1, col = 7), Pos(row = 1, col = 7))
    val result = Level2.physicalNeighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 1, col = 5), Pos(row = 1, col = 6)), Left),
      (BloxorzState(Pos(row = 1, col = 8), Pos(row = 1, col = 9)), Right),
      (BloxorzState(Pos(-1, 7), Pos(row = 0, col = 7)), Up),
      (BloxorzState(Pos(row = 2, col = 7), Pos(row = 3, col = 7)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("isLegal: Level0 (2pts)"):
    val isLegal = Level0.isLegal
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 0), Pos(row = 0, col = 0))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 0), Pos(row = 1, col = 0))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 2))), true)
    assertEquals(isLegal(BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 3))), true)
    assertEquals(isLegal(BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 4))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 3), Pos(row = 0, col = 4))), false)

  test("isLegal: Level1 (2pts)"):
    val isLegal = Level1.isLegal
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 3), Pos(row = 0, col = 4))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 2), Pos(row = 0, col = 3))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 1), Pos(row = 0, col = 2))), true)
    assertEquals(isLegal(BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 3))), true)

  test("isLegal: Level2 (2pts)"):
    val isLegal = Level2.isLegal
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 1), Pos(row = 0, col = 2))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 3), Pos(row = 0, col = 4))), false)
    assertEquals(isLegal(BloxorzState(Pos(row = 0, col = 3), Pos(row = 0, col = 3))), true)
    assertEquals(isLegal(BloxorzState(Pos(row = 2, col = 1), Pos(row = 2, col = 2))), true)
    assertEquals(isLegal(BloxorzState(Pos(row = 1, col = 3), Pos(row = 1, col = 3))), true)

  test("neighbors: Level0, start state (2pts)"):
    val state =
      BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 2)) // Not using startState to test only physicalNeighbors
    val result = Level0.neighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 2, col = 2), Pos(row = 3, col = 2)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("neighbors: Level1, start state (2pts)"):
    val state = BloxorzState(Pos(row = 1, col = 1), Pos(row = 1, col = 1))
    val result = Level1.neighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 1, col = 2), Pos(row = 1, col = 3)), Right),
      (BloxorzState(Pos(row = 2, col = 1), Pos(row = 3, col = 1)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("neighbors: Level2, start state (2pts)"):
    val state = BloxorzState(Pos(row = 1, col = 7), Pos(row = 1, col = 7))
    val result = Level2.neighbors(state)
    val expected = Seq(
      (BloxorzState(Pos(row = 2, col = 7), Pos(row = 3, col = 7)), Down)
    )
    assertEquals(result.sortBy(sortNeighbors), expected.sortBy(sortNeighbors))

  test("isDone: InfiniteLevel (2pts)"):
    val goal = InfiniteLevel.goal
    assertEquals(InfiniteLevel.isDone(BloxorzState(Pos(row = 0, col = 0), Pos(row = 0, col = 0))), false)
    assertEquals(InfiniteLevel.isDone(BloxorzState(goal, goal)), true)
    assertEquals(InfiniteLevel.isDone(BloxorzState(Pos(row = 4, col = 8), goal)), false)
    assertEquals(InfiniteLevel.isDone(BloxorzState(goal, Pos(row = 5, col = 9))), false)

  test("isDone: Level0 (2pts)"):
    val goal = Level0.goal
    assertEquals(Level0.isDone(BloxorzState(Pos(row = 0, col = 0), Pos(row = 0, col = 0))), false)
    assertEquals(Level0.isDone(BloxorzState(goal, goal)), true)
    assertEquals(Level0.isDone(BloxorzState(Pos(row = 1, col = 2), goal)), false)

  test("isDone: Level1 (2pts)"):
    val goal = Level1.goal
    assertEquals(Level1.isDone(BloxorzState(Pos(row = 0, col = 0), Pos(row = 0, col = 0))), false)
    assertEquals(Level1.isDone(BloxorzState(goal, goal)), true)
    assertEquals(Level1.isDone(BloxorzState(goal, Pos(row = 4, col = 8))), false)
    assertEquals(Level1.isDone(BloxorzState(Pos(row = 4, col = 6), goal)), false)

  test("isDone: Level2 (2pts)"):
    val goal = Level2.goal
    assertEquals(Level2.isDone(BloxorzState(Pos(row = 0, col = 0), Pos(row = 0, col = 0))), false)
    assertEquals(Level2.isDone(BloxorzState(goal, goal)), true)
    assertEquals(Level2.isDone(BloxorzState(goal, Pos(row = 3, col = 2))), false)
    assertEquals(Level2.isDone(BloxorzState(Pos(row = 3, col = 0), goal)), false)

  import scala.concurrent.duration.*
  override val munitTimeout = 20.seconds
