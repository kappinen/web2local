package types

import io.LocalStorage._
import types.DataItem

/**
 * Created by Evgeni Kappinen on 5/24/14.
 */
object DataItemExtension {

  implicit class DataItemHelper(sequence: Seq[DataItem]) {

    def applyTo(newKey:String)(func: (DataItem) => Any): Seq[DataItem] = sequence.map((a) => a ++ Map(newKey -> func(a)))

    def minMaxBy(key: String): Pair[Double, Double] =
      sequence.foldLeft[Pair[Double, Double]](Pair(1, 0))((a, b) => (math.min(b(key).toDouble, a._1), math.max(b(key).toDouble, a._2)))

    def meanBy(key: String): Double = sequence.toDouble(key).reduce((a, b) => b + a) / sequence.size

    def diffBy(key: String): Seq[DataItem] =
      (sequence.drop(1) zip sequence.dropRight(1)).
        map((a) => a._1 ++ Map(key -> (a._2(key).toDouble - a._1(key).toDouble)))

    def normilize(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - limits._1) / (limits._2 - limits._1)))
    }

    def zscore(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)
      val mean = meanBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - mean) / (limits._2 - limits._1)))
    }

    def toDouble(key: String): Seq[Double] = sequence.map((a) => a(key).toDouble)
  }
}

