package cs214
package grid

import scala.collection.mutable

import scalajs.js
import scalajs.js.JSConverters.*
import org.scalajs.dom.document
import org.scalajs.dom.html
import org.scalajs.dom.window
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.WebGLRenderingContext

import pathfinder.*
import pathfinder.grid.*

object Bloxorz3D:

  val levels: Map[Int, BloxorzLevel & StringParserTerrain] = Map(
    0 -> BloxorzLevels.Level0,
    1 -> BloxorzLevels.Level1,
    2 -> BloxorzLevels.Level2
  )

  var currentLevelId = 0
  def currentLevel = levels(currentLevelId)
  var currentLevel3D = BloxorzLevel3D(currentLevel.level)
  var isDisplayingSolution = false

  def main(args: Array[String]): Unit =
    val canvas = document.querySelector("#glcanvas").asInstanceOf[html.Canvas]
    val gl = canvas.getContext("webgl").asInstanceOf[WebGLRenderingContext]

    val renderer = Renderer(gl)
    setupKeyboardEvents(renderer)
    window.setTimeout(() => currentLevel3D.render(renderer), 500)
    displayGraph(currentLevel)

  def setupKeyboardEvents(renderer: Renderer): Unit =
    document.addEventListener(
      "keydown",
      e =>
        e.preventDefault()
        e.asInstanceOf[KeyboardEvent].key match
          case "ArrowDown" | "s"  => if !isDisplayingSolution then move(BloxorzMove.Down, renderer)
          case "ArrowUp" | "w"    => if !isDisplayingSolution then move(BloxorzMove.Up, renderer)
          case "ArrowRight" | "d" => if !isDisplayingSolution then move(BloxorzMove.Right, renderer)
          case "ArrowLeft" | "a"  => if !isDisplayingSolution then move(BloxorzMove.Left, renderer)
          case "r" =>
            currentLevel3D.reset()
            currentLevel3D.render(renderer)
          case "Enter" =>
            isDisplayingSolution = true
            def progress(seq: List[BloxorzMove]): Unit = seq match
              case head :: next =>
                window.setTimeout(() => progress(next), 500)
                move(head, renderer, false)
              case Nil =>
                currentLevel3D.reset()
                currentLevel3D.render(renderer)
                isDisplayingSolution = false
            currentLevel3D.reset()
            currentLevel3D.render(renderer)
            window.setTimeout(() => progress(Solver(currentLevel).solution), 500)
          case _ =>
    )

  def move(dir: BloxorzMove, renderer: Renderer, goToNextLevel: Boolean = true) =
    currentLevel3D.move(dir)
    currentLevel3D.render(renderer)
    if goToNextLevel && currentLevel3D.isDone(currentLevel3D.state) then
      currentLevelId = (currentLevelId + 1) % levels.size
      currentLevel3D = BloxorzLevel3D(currentLevel.level)
      currentLevel3D.render(renderer)
      displayGraph(levels(currentLevelId))

  def displayGraph(level: StringParserTerrain & BloxorzDef) =
    val graph = LevelGraph(level)

    def id(b: BloxorzState) =
      (b.b1.col, b.b1.row, b.b2.col, b.b2.row).toString

    val visited = mutable.Set(level.startState)
    val used = mutable.Set.empty[(BloxorzState, BloxorzState, BloxorzMove)]
    var toVisit = mutable.Set(level.startState)

    def jsData() =
      val nodes = js.Array(graph.graph.nodes.map(b =>
        js.Dictionary[Any](
          "posX" -> ((b.asInstanceOf[BloxorzState].b1.col + b.asInstanceOf[
            BloxorzState
          ].b2.col) / 2.0 + 0.5),
          "posY" -> ((b.asInstanceOf[BloxorzState].b1.row + b.asInstanceOf[
            BloxorzState
          ].b2.row) / 2.0 + 0.5),
          "x1" -> b.asInstanceOf[BloxorzState].b1.col,
          "x2" -> b.asInstanceOf[BloxorzState].b2.col,
          "y1" -> b.asInstanceOf[BloxorzState].b1.row,
          "y2" -> b.asInstanceOf[BloxorzState].b2.row,
          "id" -> id(b.asInstanceOf[BloxorzState]),
          "description" -> (
            if b.asInstanceOf[BloxorzState].isStanding && b.asInstanceOf[BloxorzState].b1 == level.startPos
            then "start"
            else if b.asInstanceOf[BloxorzState].isStanding && b.asInstanceOf[BloxorzState].b1 == level.goal
            then "goal"
            else if b.asInstanceOf[BloxorzState].isStanding then "standing"
            else "laying"
          ),
          "visited" -> visited.contains(b.asInstanceOf[BloxorzState])
        )
      ).toSeq*).sortBy(_("id").asInstanceOf[String])

      val links = graph.graph.transitions.map((from, to, move) =>
        js.Dictionary[Any](
          "source" -> id(from.asInstanceOf[BloxorzState]),
          "target" -> id(to.asInstanceOf[BloxorzState]),
          "used" -> used.contains((
            from.asInstanceOf[BloxorzState],
            to.asInstanceOf[BloxorzState],
            move.asInstanceOf[BloxorzMove]
          ))
        )
      ).toJSArray

      val background = graph.getTerrain.map((i, j) =>
        js.Dictionary[Any](
          "y" -> i,
          "x" -> j
        )
      ).toJSArray

      (nodes, links, background)

    val (nodes, links, background) = jsData()
    val visualization = GraphVisualization(nodes, links, background)

    document.querySelector("#animate").addEventListener(
      "click",
      e =>
        val toVisitNext = mutable.Set.empty[BloxorzState]
        for b <- toVisit do
          for next <- currentLevel.neighbors(b) do
            if !visited.contains(next._1.asInstanceOf[BloxorzState]) then
              toVisitNext.add(next._1.asInstanceOf[BloxorzState])
              visited.add(next._1.asInstanceOf[BloxorzState])
              used.add((b, next._1, next._2))

        toVisit = toVisitNext

        val (nodes, links, _) = jsData()

        visualization.animate(nodes, links)
    )

    document.querySelector("#toggle-layout").addEventListener("click", e => visualization.toggleLayout())
