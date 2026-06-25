package cs214.grid

import pathfinder.GameDef
import pathfinder.grid.StringParserTerrain

import scalajs.js
import scala.scalajs.js
import js.JSConverters.*

class LevelGraph[State, Move](val level: GameDef[State, Move] & StringParserTerrain):
  case class Graph(nodes: Set[State], transitions: List[(State, State, Move)])

  val graph = getGraph

  def getGraph: Graph =
    def explore(from: State, acc: Graph): Graph =
      level.neighbors(from).foldLeft(acc)((graph, b) =>
        if graph.nodes.contains(b._1) then Graph(graph.nodes, (from, b._1, b._2) :: graph.transitions)
        else explore(b._1, Graph(graph.nodes + b._1, (from, b._1, b._2) :: graph.transitions))
      )

    explore(level.startState, Graph(Set.empty, Nil))

  def getTerrain: Seq[(Int, Int)] =
    level.vector.zipWithIndex
      .flatMap((v, i) => v.zipWithIndex.collect { case (v, j) if Seq('o', 'S', 'T').contains(v) => (i, j) })
