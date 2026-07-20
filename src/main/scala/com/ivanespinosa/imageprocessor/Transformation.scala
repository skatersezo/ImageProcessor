package com.ivanespinosa.imageprocessor

trait Transformation {

  def apply(image: Image): Image
}
