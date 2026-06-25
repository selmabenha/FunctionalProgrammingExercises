package scalashop.image

import scalashop.common.*

/** Sobel edge detection filter, used to detect the horizontal and vertical
  * edges of an image. Take a look at `Kernel.sobelX` and `Kernel.sobelY` for
  * default kernels for this filter.
  */
class SobelEdgeDetection(src: Image, kernelX: Kernel, kernelY: Kernel)
    extends Image(src.height, src.width):
  require((kernelX.width, kernelX.height) == (kernelY.width, kernelY.height))

  val bwSrc = Grayscale(src)
  val xConvo = Convolution(bwSrc, kernelX)
  val yConvo = Convolution(bwSrc, kernelY)

  def apply(x: Int, y: Int): Pixel =
    // Keep only 1 color channel as they're all the same (black and white)
    val xVal = xConvo(x, y)._2
    val yVal = yConvo(x, y)._2
    val grayScale = math.sqrt(xVal * xVal + yVal * yVal).toInt

    argb(255, grayScale, grayScale, grayScale)
