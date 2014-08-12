package types


case class DataItem(source: String, dtime: Long, tags: List[String], data: Map[String, Any]) {

  def apply [DataItem <: String](key: String) : String = data(key).toString
  def ++(newData: Map[String, Any]) = new DataItem(source, dtime, tags, data ++ newData)

  def toDouble(key: String): Double = data(key).toString.replaceAll(" ", "").replaceAll(",", ".").toDouble
}
