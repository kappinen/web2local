package types

/**
 * Created by home on 5/24/14.
 */
object DataItemExtension {

  implicit class DataItemHelper(sequence: Seq[DataItem]) {

    def getMinMax(key: String): Pair[Double, Double] =
      sequence.foldLeft[Pair[Double, Double]](Pair(1, 0))((a, b) => (math.min(b(key).toDouble, a._1), math.max(b(key).toDouble, a._2)))

    def diffBy(key: String): Seq[DataItem] =
      (sequence.drop(1) zip sequence.dropRight(1)).
        map((a) => a._1 ++ Map(key -> (a._2(key).toDouble - a._1(key).toDouble)))

    def normilize(key: String): Seq[DataItem] = {
      val limits = getMinMax(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - limits._1) / (limits._2 - limits._1)))
    }

    def as[T](key: String): Seq[T] = sequence.map((a) => a(key).asInstanceOf[T])

  }
}

