package scalashop

import scalashop.common.*
import scalashop.image.*

import java.util.concurrent.*
import scala.collection.*

import util.Random

class GrayscaleSuite extends scalashop.ImageTestSuite:

  // safety/invariance tests
  testDimensions.foreach((h, w) =>
    test(s"grayscale: white image of size $h x $w should be unchanged") {
      assertApproxEqual(pureColor(h, w, 0xffffffff), Grayscale(pureColor(h, w, 0xffffffff)))
    }
    test(s"grayscale: black image of size $h x $w should be unchanged") {
      assertApproxEqual(pureColor(h, w, 0x00000000), Grayscale(pureColor(h, w, 0x00000000)))
    }
    test(s"grayscale: randomized grayscale image of size $h x $w should be unchanged") {
      val source: Image = randomMonochrome(h, w).build
      assertApproxEqual(source, Grayscale(source))
    }
  )

  // correctness tests
  /** Some colors and their grayscale versions */
  val colorPairs = Seq(
    argb(255, 255, 0, 0) -> argb(255, 76, 76, 76), // red
    argb(255, 0, 255, 0) -> argb(255, 149, 149, 149), // green
    argb(255, 0, 0, 255) -> argb(255, 29, 29, 29), // blue
    argb(65, 106, 9, 189) -> argb(65, 58, 58, 58), // random colors and their grayscale versions
    argb(219, 13, 216, 132) -> argb(219, 145, 145, 145),
    argb(53, 126, 193, 173) -> argb(53, 170, 170, 170),
    argb(161, 102, 181, 85) -> argb(161, 146, 146, 146),
    argb(43, 239, 16, 33) -> argb(43, 84, 84, 84),
    argb(211, 62, 227, 192) -> argb(211, 173, 173, 173),
    argb(141, 117, 205, 54) -> argb(141, 161, 161, 161),
    argb(205, 18, 174, 236) -> argb(205, 134, 134, 134),
    argb(137, 96, 218, 44) -> argb(137, 161, 161, 161),
    argb(37, 211, 255, 243) -> argb(37, 240, 240, 240)
  )

  testDimensions.foreach((h, w) =>
    test(s"grayscale: red image of size $h x $w should transform correctly") {
      assertApproxEqual(
        pureColor(h, w, colorPairs(0)._2),
        Grayscale(pureColor(h, w, colorPairs(0)._1))
      )
    }
    test(s"grayscale: green image of size $h x $w should transform correctly") {
      assertApproxEqual(
        pureColor(h, w, colorPairs(1)._2),
        Grayscale(pureColor(h, w, colorPairs(1)._1))
      )
    }
    test(s"grayscale: blue image of size $h x $w should transform correctly") {
      assertApproxEqual(
        pureColor(h, w, colorPairs(2)._2),
        Grayscale(pureColor(h, w, colorPairs(2)._1))
      )
    }
  )

  def pickRandom[A](seq: Seq[A]): A =
    seq(Random.nextInt(seq.length))

  (1 to 10).foreach(i =>
    // generate a test case
    val (sourceArray, targetArray) = Array.fill(9)(pickRandom(colorPairs)).unzip
    val (sourceImage, targetImage) = (Image(3, 3, sourceArray), Image(3, 3, targetArray))
    test("grayscale: 3x3 random image should transform correctly") {
      assertApproxEqual(targetImage, Grayscale(sourceImage))
    }
  )
