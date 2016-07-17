package types

/**
 * Created by Evgeni Kappinen on 5/24/14.
 */
object DataItemExtension {

  implicit class DataItemHelper(sequence: Seq[DataItem]) {

    def apply(key: String): Seq[Any] = sequence.map((a) => a(key))

    def applyTo(newKey: String)(func: (DataItem) => Any): Seq[DataItem] = sequence.map((a) => a ++ Map(newKey -> func(a)))

    def slidingBy(newKey: String, window: Int)(func: (Seq[DataItem]) => Any): Seq[DataItem] = sequence.sliding(window).toSeq.map((a) => a.last ++ Map(newKey -> func(a)))


    def minMaxBy(key: String): Pair[Double, Double] =
      sequence.foldLeft[Pair[Double, Double]](Pair(1, 0))((a, b) => (math.min(b(key).toDouble, a._1), math.max(b(key).toDouble, a._2)))

    //TODO: , size: Int = 2 and slideBy
    def meanBy(key: String): Double = sequence.toDouble(key).reduce((a, b) => b + a) / sequence.size

    def diffBy(key: String): Seq[DataItem] =
      (sequence.drop(1) zip sequence.dropRight(1)).
        map((a) => a._1 ++ Map(key -> (a._2.toDouble(key) - a._1.toDouble(key))))

    def normilize(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - limits._1) / (limits._2 - limits._1)))
    }

    def zscore(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)
      val mean = meanBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - mean) / (limits._2 - limits._1)))
    }

    def toDouble(key: String): Seq[Double] = sequence.map((a) => a.toDouble(key))

    def getOrDefault[T](key: String, defaultValue: T): Seq[T] = {
      sequence.map((a) => {
        if (a(key) == null) {
          defaultValue
        } else {
          a(key).asInstanceOf[T]
        }
      })
    }

  }

  //http://rosettacode.org/wiki/Averages/Median
  def median(sequence: Array[Double]): Double = {
    val sorted = sequence.sorted
    val splitted = sorted.splitAt(sorted.size / 2)
    if (sorted.size % 2 == 0) {
      return (splitted._1.last + splitted._2.head) / 2d
    } else {
      splitted._2.head
    }
  }

}

