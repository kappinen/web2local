package sources

import types.DataItem
import io.LocalStorage._
import types.DataItem

/**
 * Created by home on 2/16/14.
 */
abstract class DataSourceScala extends DataSource {

  protected def parse(fetchData: (String) => String)(url: String): DataItem

  protected def parseItems(fetchData: (String) => String)(opts: Map[String, String]): Seq[DataItem]


  def gitem(criteria: String): DataItem = {
    parse((url)=> scala.io.Source.fromURL(url).mkString)(criteria)
  }

  def gitems(criteria: Map[String, String]): Seq[DataItem] = {
    parseItems((url) => scala.io.Source.fromURL(url).mkString)(criteria)
  }
}
