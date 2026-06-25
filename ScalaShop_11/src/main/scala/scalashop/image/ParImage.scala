package scalashop.image

import scalashop.common.*

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.collection.parallel.ForkJoinTaskSupport

private def availableProcessors = sys.runtime.availableProcessors()

final class ParImage(
    src: Image,
    private var parallelization: Int = availableProcessors
) extends Image(src.height, src.width):

  private def buildSequential(
      destination: ArrayImage,
      xFrom: Int,
      xTo: Int,
      yFrom: Int,
      yTo: Int
  ): Unit = {
    for y <- yFrom until yTo
        x <- xFrom until xTo do
      destination(x, y) = this(x, y)
  }

  override def build: ArrayImage =
    // compute the collection to work on
    val stepSize = (height + parallelization - 1) / parallelization
    val splits: Seq[(Int, Int)] = (0 until height by stepSize).map { startY =>
      (startY, math.min(startY + stepSize, height))
    } // type is provided just so it compiles, feel free to change it
    val parSplits = splits.par

    parSplits.tasksupport = ForkJoinTaskSupport(
      ForkJoinPool(parallelization)
    ) // make sure we apply the desired level of parallelism

    val destination = ArrayImage(height, width)

    // perform your computation in parallel
    parSplits.foreach { case (startY, endY) =>
      buildSequential(destination, 0, width, startY, endY)
    }
    // return the constructed image
    destination

  def apply(x: Int, y: Int): Pixel = src(x, y)

  override def seq: Image = src.seq // recursively eliminate parallelization
  override def par: ParImage = par(availableProcessors)
  override def par(n: Int): ParImage =
    require(n >= 1)
    if n == parallelization then this
    else ParImage(src, n)
