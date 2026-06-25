package pathfinder

import grid.TestLevels.*
import grid.TestMove.*
import grid.*

class SolverUnitSuite extends munit.FunSuite:
  def sortNeighborsHistory(entry: (TestState, List[TestMove])) = entry._2.headOption.map(_.ordinal + 1).getOrElse(0)

  test("neighborsWithHistory: Level0, Pos(row=2, col=3) (4pts)"):
    val state = TestState(Pos(row = 2, col = 3))
    val history = List(Right, Down)
    val nWithHistory = Solver(Level0).neighborsWithHistory(state, history)
    val expected = List(
      state.left -> (Left +: history),
      state.down -> (Down +: history),
      state.up -> (Up +: history)
    )
    assertEquals(nWithHistory.toList.sortBy(sortNeighborsHistory), expected.sortBy(sortNeighborsHistory))

  test("neighborsWithHistory: Level0, Pos(row=1, col=3) (4pts)"):
    val state = TestState(Pos(row = 1, col = 3))
    val history = List(Right)
    val nWithHistory = Solver(Level0).neighborsWithHistory(state, history)
    val expected = List(
      state.left -> (Left +: history),
      state.down -> (Down +: history)
    )
    assertEquals(nWithHistory.toList.sortBy(sortNeighborsHistory), expected.sortBy(sortNeighborsHistory))

  test("neighborsWithHistory: Level1, Pos(row=1, col=1) (4pts)"):
    val state = TestState(Pos(row = 1, col = 1))
    val history = Nil
    val nWithHistory = Solver(Level1).neighborsWithHistory(state, history)
    val expected = List(
      state.right -> (Right +: history),
      state.left -> (Left +: history),
      state.up -> (Up +: history),
      state.down -> (Down +: history)
    )
    assertEquals(nWithHistory.toList.sortBy(sortNeighborsHistory), expected.sortBy(sortNeighborsHistory))

  test("neighborsWithHistory: Level2, Pos(row=0, col=3) (4pts)"):
    val state = TestState(Pos(row = 0, col = 3))
    val history = List(Down, Left, Left, Up, Left, Left, Up, Up, Up)
    val nWithHistory = Solver(Level2).neighborsWithHistory(state, history)
    val expected = List(
      state.down -> (Down +: history)
    )
    assertEquals(nWithHistory.toList.sortBy(sortNeighborsHistory), expected.sortBy(sortNeighborsHistory))

  test("newNeighborsOnly: Level0, empty neighbors (2pts)"):
    val history = List(Right, Down)
    val nWithHistory = LazyList.empty[(TestState, List[TestMove])]
    val explored = Set(
      TestState(Pos(row = 0, col = 0))
    )
    val newNeighbors = Solver(Level0).newNeighborsOnly(nWithHistory, explored)
    val expected = nWithHistory
    assertEquals(newNeighbors.toList.sortBy(sortNeighborsHistory), expected.toList.sortBy(sortNeighborsHistory))

  test("newNeighborsOnly: Level0, nothing explored (2pts)"):
    val history = List(Right, Down)
    val nWithHistory = LazyList(
      TestState(Pos(row = 2, col = 2)) -> (Left +: history),
      TestState(Pos(row = 3, col = 3)) -> (Down +: history),
      TestState(Pos(row = 1, col = 3)) -> (Up +: history)
    )
    val explored = Set.empty[TestState]
    val newNeighbors = Solver(Level0).newNeighborsOnly(nWithHistory, explored)
    val expected = nWithHistory
    assertEquals(newNeighbors.toList.sortBy(sortNeighborsHistory), expected.toList.sortBy(sortNeighborsHistory))

  test("newNeighborsOnly: Level0, one irrelevant state explored (2pts)"):
    val history = List(Right, Down)
    val nWithHistory = LazyList(
      TestState(Pos(row = 2, col = 2)) -> (Left +: history),
      TestState(Pos(row = 3, col = 3)) -> (Down +: history),
      TestState(Pos(row = 1, col = 3)) -> (Up +: history)
    )
    val explored = Set(
      TestState(Pos(row = 0, col = 0))
    )
    val newNeighbors = Solver(Level0).newNeighborsOnly(nWithHistory, explored)
    val expected = nWithHistory
    assertEquals(newNeighbors.toList.sortBy(sortNeighborsHistory), expected.toList.sortBy(sortNeighborsHistory))

  test("newNeighborsOnly: Level0, one state explored (2pts)"):
    val history = List(Right, Down)
    val nWithHistory = LazyList(
      TestState(Pos(row = 2, col = 2)) -> (Left +: history),
      TestState(Pos(row = 3, col = 3)) -> (Down +: history),
      TestState(Pos(row = 1, col = 3)) -> (Up +: history)
    )
    val explored = Set(
      TestState(Pos(row = 2, col = 2))
    )
    val newNeighbors = Solver(Level0).newNeighborsOnly(nWithHistory, explored)
    val expected = LazyList(
      TestState(Pos(row = 3, col = 3)) -> (Down +: history),
      TestState(Pos(row = 1, col = 3)) -> (Up +: history)
    )
    assertEquals(newNeighbors.toList.sortBy(sortNeighborsHistory), expected.toList.sortBy(sortNeighborsHistory))

  test("newNeighborsOnly: Level0, all states explored (2pts)"):
    val history = List(Right, Down)
    val nWithHistory = LazyList(
      TestState(Pos(row = 2, col = 2)) -> (Left +: history),
      TestState(Pos(row = 3, col = 3)) -> (Down +: history),
      TestState(Pos(row = 1, col = 3)) -> (Up +: history)
    )
    val explored = Set(
      TestState(Pos(row = 2, col = 2)),
      TestState(Pos(row = 3, col = 3)),
      TestState(Pos(row = 1, col = 3))
    )
    val newNeighbors = Solver(Level0).newNeighborsOnly(nWithHistory, explored)
    val expected = LazyList.empty[(TestState, List[TestMove])]
    assertEquals(newNeighbors.toList.sortBy(sortNeighborsHistory), expected.toList.sortBy(sortNeighborsHistory))

  /** Tests the methods returning a list of paths and state. Note that expected
    * accpets a Set of paths and not just one path because multiple paths are
    * possible to go to a same point. It represents the set of all accepted
    * paths
    */
  def testFrom(result: LazyList[(TestState, List[TestMove])], expected: Seq[(TestState, Set[List[TestMove]])]) =
    var maxLength = 0
    for path <- result do
      val currentLength = path._2.length
      assert(currentLength >= maxLength, "from: The returned paths should be sorted by ascending path length")
      maxLength = currentLength

    assertEquals(result.length, expected.length)

    val resultMap = result.toMap
    val expectedMap = expected.toMap
    for (s, path) <- resultMap do
      assert(expectedMap.contains(s), f"from: Missing a reachable state ${s}")
      assert(expectedMap(s).contains(path), f"from: Incorrect path ${path} to state ${s}")

  test("from: Level0, start state (4pts)"):
    val initialState = TestState(Pos(row = 1, col = 2))
    val initial = LazyList(
      TestState(Pos(row = 1, col = 2)) -> Nil
    )
    val explored = Set(
      initialState
    )

    val expected = Seq(
      initialState -> Set(Nil),
      TestState(Pos(row = 1, col = 3)) -> Set(List(Right)),
      TestState(Pos(row = 2, col = 2)) -> Set(List(Down)),
      TestState(Pos(row = 2, col = 3)) -> Set(List(Down, Right), List(Right, Down)),
      TestState(Pos(row = 3, col = 2)) -> Set(List(Down, Down)),
      TestState(Pos(row = 3, col = 3)) -> Set(List(Down, Down, Right), List(Down, Right, Down), List(Right, Down, Down))
    )

    testFrom(Solver(Level0).from(initial, explored), expected)

  test("from: Level3, start state (4pts)"):
    val initialState = TestState(Pos(row = 1, col = 2))
    val initial = LazyList(
      TestState(Pos(row = 1, col = 2)) -> Nil
    )
    val explored = Set(
      initialState
    )

    val expected = Seq(
      initialState -> Set(Nil),
      TestState(Pos(row = 1, col = 1)) -> Set(List(Left)),
      TestState(Pos(row = 0, col = 1)) -> Set(List(Up, Left)),
      TestState(Pos(row = 2, col = 1)) -> Set(List(Down, Left)),
      TestState(Pos(row = 0, col = 0)) -> Set(List(Left, Up, Left)),
      TestState(Pos(row = 2, col = 0)) -> Set(List(Left, Down, Left)),
      TestState(Pos(row = 3, col = 1)) -> Set(List(Down, Down, Left)),
      TestState(Pos(row = 3, col = 2)) -> Set(List(Right, Down, Down, Left))
    )

    testFrom(Solver(Level3).from(initial, explored), expected)

  test("pathsFromStart: Level0 (2pts)"):
    val expected = Seq(
      TestState(Pos(row = 1, col = 2)) -> Set(Nil),
      TestState(Pos(row = 1, col = 3)) -> Set(List(Right)),
      TestState(Pos(row = 2, col = 2)) -> Set(List(Down)),
      TestState(Pos(row = 2, col = 3)) -> Set(List(Down, Right), List(Right, Down)),
      TestState(Pos(row = 3, col = 2)) -> Set(List(Down, Down)),
      TestState(Pos(row = 3, col = 3)) -> Set(List(Down, Down, Right), List(Down, Right, Down), List(Right, Down, Down))
    )
    testFrom(Solver(Level0).pathsFromStart, expected)

  test("pathsFromStart: Level3 (2pts)"):
    val expected = Seq(
      TestState(Pos(row = 1, col = 2)) -> Set(Nil),
      TestState(Pos(row = 1, col = 1)) -> Set(List(Left)),
      TestState(Pos(row = 0, col = 1)) -> Set(List(Up, Left)),
      TestState(Pos(row = 2, col = 1)) -> Set(List(Down, Left)),
      TestState(Pos(row = 0, col = 0)) -> Set(List(Left, Up, Left)),
      TestState(Pos(row = 2, col = 0)) -> Set(List(Left, Down, Left)),
      TestState(Pos(row = 3, col = 1)) -> Set(List(Down, Down, Left)),
      TestState(Pos(row = 3, col = 2)) -> Set(List(Right, Down, Down, Left))
    )

    testFrom(Solver(Level3).pathsFromStart, expected)

  test("pathsToGoal: Level0 (2pts)"):
    val expected = Seq(
      TestState(Pos(row = 1, col = 3)) -> Set(List(Right))
    )

    testFrom(Solver(Level0).pathsToGoal, expected)

  test("pathsToGoal: Level3 (2pts)"):
    val expected = Seq(
      TestState(Pos(row = 3, col = 2)) -> Set(List(Right, Down, Down, Left))
    )

    testFrom(Solver(Level3).pathsToGoal, expected)

class SolverSystemSuite extends munit.FunSuite:
  def testSolution(solver: Solver[TestState, TestMove], checker: SolutionChecker, expected: Seq[TestMove]) =
    require(checker.checkSolution(expected))
    val result = solver.solution
    assert(result.length <= expected.length, "solution is not of optimal length")
    assert(checker.checkSolution(result), "solution does not lead to the goal")

  test("solution: Level0 (2pts)"):
    val expected = Seq(Right)
    testSolution(Solver(Level0), SolutionChecker(Level0), expected)

  test("solution: Level1 (2pts)"):
    val expected = Seq(Right, Right, Right, Right, Down, Right, Right, Down, Down)
    testSolution(Solver(Level1), SolutionChecker(Level1), expected)

  test("solution: Level2 (2pts)"):
    val expected = Seq(Up, Left, Left, Down, Left, Left, Left, Left)
    testSolution(Solver(Level2), SolutionChecker(Level2), expected)

  test("solution: Level3 (2pts)"):
    val expected = Seq(Left, Down, Down, Right)
    testSolution(Solver(Level3), SolutionChecker(Level3), expected)
