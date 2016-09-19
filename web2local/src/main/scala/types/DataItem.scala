package types

import common.Utils._


//TODO: data map -> listMap to preserve the order
case class DataItem(source: String, dtime: Long, tags: List[String], data: Map[String, Any]) {

  def apply[DataItem <: String](key: String): String = data(key).toString

  def ++(newData: Map[String, Any]) = new DataItem(source, dtime, tags, data ++ newData)

  def toDouble(key: String): Double = common.Utils.toDouble(data(key))

  def toMillis(key: String): Double = (str2date(data(key).toString).getMillis.toDouble / (24 * 60 * 60 * 1000d)).toLong.toDouble

  def headers(): Set[String] = data.keySet
}
