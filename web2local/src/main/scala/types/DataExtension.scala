package types

import scala.collection.GenIterable

/**
 * Created by evka on 19.4.2016.
 */
object DataExtension {

  implicit class DataExtensionHelper(sequence: GenIterable[Double]) {
    def -(traversable: GenIterable[Double]): GenIterable[Double] =
      sequence.zip(traversable).map(a => a._1 - a._2)

    def +(traversable: GenIterable[Double]): GenIterable[Double] =
      sequence.zip(traversable).map(a => a._1 + a._2)

    def *(traversable: GenIterable[Double]): GenIterable[Double] =
      sequence.zip(traversable).map(a => a._1 * a._2)

    def /(traversable: GenIterable[Double]): GenIterable[Double] =
      sequence.zip(traversable).map(a => a._1 / a._2)

    def -(value: Double): GenIterable[Double] = sequence.map(a => a - value)

    def +(value: Double): GenIterable[Double] = sequence.map(a => a + value)

    def *(value: Double): GenIterable[Double] = sequence.map(a => a * value)

    def /(value: Double): GenIterable[Double] = sequence.map(a => a / value)

    def ndiff(): GenIterable[Double] = (sequence.drop(1) zip sequence.toArray.dropRight(1)).map((a) => a._2 - a._1)

    def mean(): Double = sequence.sum / sequence.size

    def stdev(): Double = math.sqrt(variance())
    def variance(): Double = {
      val mean = sequence.mean()
      sequence.map(a => math.pow(a - mean, 2)).sum / sequence.size
    }

    def unbiasvariance(): Double = {
      val mean = sequence.mean()
      sequence.map(a => math.pow(a - mean, 2)).sum / (sequence.size -1).toDouble
    }

    def unbiasstdev(): Double = math.sqrt(unbiasvariance())

  }

}
