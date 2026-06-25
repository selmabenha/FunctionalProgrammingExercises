package pathfinder

/** Represents the definition of a game */
trait GameDef[State, Move]:

  /** This function returns the state at the start position of the game. */
  def startState: State

  /** Returns the list of positions reachable from the current state */
  def neighbors(state: State): Seq[(State, Move)]

  /** Returns `true` this state is at a final position */
  def isDone(state: State): Boolean
