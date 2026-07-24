package com.ivanespinosa.imageprocessor

trait Transformation {

  def apply(image: Image): Image
}

case class Crop(x: Int, y: Int, w: Int, h: Int) extends Transformation {

  override def apply(image: Image): Image =
    try {
      image.crop(x, y, w, h)
    } catch {
      case _: Exception =>
        println(s"Error: coordinates are out of bounds. Max coordinates: ${image.width} ${image.height}")
        image
    }
}

case object Grayscale extends Transformation {
  override def apply(image: Image): Image =
    image.map { pixel =>
      val avg = (pixel.red + pixel.green + pixel.blue) / 3
      Pixel(avg, avg, avg)
    }
}

case object Invert extends Transformation {
  override def apply(image: Image): Image =
    image.map { pixel =>
      Pixel(
        255 - pixel.red,
        255 - pixel.green,
        255 - pixel.blue
      )
    }
}

case class Colorize(color: Pixel) extends Transformation {
  override def apply(image: Image): Image =
    image.map { pixel =>
      val avg = (pixel.red + pixel.green + pixel.blue) / 3
      Pixel(
        (color.red * (avg / 255.0)).toInt,
        (color.green * (avg / 255.0)).toInt,
        (color.blue * (avg / 255.0)).toInt,
      )
    }
}

// brightness, contrast
// hue, saturation, lightness
// levels, curves

case class Blend(fgImage: Image, blendMode: BlendMode) extends Transformation {
  override def apply(bgImage: Image): Image = {
    if fgImage.width != bgImage.width || fgImage.height != bgImage.height then {
      println(s"Error: images don't have the same sizes: ${fgImage.width} x ${fgImage.height} vs ${bgImage.width} x ${bgImage.height}")
      bgImage
    } else {
      val width = fgImage.width
      val height = fgImage.height
      val result = Image.black(width, height)

      for {
        x <- 0 until width
        y <- 0 until height
      } do result.setColor(
        x,
        y,
        blendMode.combine(
          fgImage.getColor(x, y),
          bgImage.getColor(x, y)
        )
      )

      result
    }
  }
}

object Transformation {

  def main(args: Array[String]): Unit = {

//    val islands = Image.loadResource("islands.png")
//    val islands_grayscale = Grayscale(islands)
//    islands_grayscale.saveResource("islands_grayscale.jpg")
//    Invert(islands).saveResource("islands_invert.jpg")
//    Colorize(Pixel.GREEN)(islands).saveResource("islands_colorized_green.jpg")

    val isl = Image.loadResource("islands_cropped.jpg")
    val str = Image.loadResource("storm_cropped.jpg")

    // Blend(str, Multiply)(isl).saveResource("multiply_island_storm.jpg")
    // Blend(str, Transparency(0.3))(isl).saveResource("transparency_island_storm.jpg")
    Blend(str, Overlay)(isl).saveResource("overlay_island_storm.jpg")
  }
}