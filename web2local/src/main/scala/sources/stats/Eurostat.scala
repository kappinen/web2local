package sources.stats

import sources.{DataSourceScala, DataSourceJsoup, DataSourceWebDriver}
import org.jsoup.nodes.Document
import types.{DataItemUtil, DataItem}
import org.jsoup.Jsoup
import scala.io.Source._
import java.util
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import types.DataItem
import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.reflect.io.File

/**
 * Created by Evgeni Kappinen on 2/10/14.
 */
class Eurostat extends DataSourceScala {
  override val name: String = "Eurostat"

  implicit def array2data(x: String): Boolean = Boolean.unbox(x)

  protected def parseItems(fetchData: (String) => String)(opts: Map[String, String]): Seq[DataItem] = {

    val data: String = fetchData(opts.get("url").get)
    val mapper = new ObjectMapper()

    //convert JSON string to Map
    val map2: util.HashMap[String, Object] =
      mapper.readValue(data, new TypeReference[util.HashMap[String, Object]]() {});

    val res: java.util.ArrayList[java.util.LinkedHashMap[String, Object]] =
      map2.get("value").asInstanceOf[java.util.ArrayList[java.util.LinkedHashMap[String, Object]]]

    var retSeq = Seq[DataItem]();
    val iterator = res.iterator()
    while (iterator.hasNext) {
      val map = iterator.next().asScala
      map.map((a) => scala.collection.immutable.Map(a._1.asInstanceOf[String] -> a._2.toString.asInstanceOf[Any]))

      retSeq = retSeq :+ DataItem(name, System.currentTimeMillis(), List[String](),
        map.map((a) => (a._1.asInstanceOf[String] -> a._2.toString.asInstanceOf[Any])).toMap)
    }

    if (opts.getOrElse[String]("list", "false").toBoolean) {
      retSeq.foreach((a) => println(a.data("DatasetCode") + ":" + a.data("DatasetTitle")))
    }

    retSeq
  }


  override protected def parse(fetchData: (String) => String)(url: String): DataItem =
    parseItems(fetchData)(Map("url" -> url)).head


  private def example_odata() = {
//    import sources.stats.Eurostat
    val urlList = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/GetDatasets?$format=json"
    val urlData = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/REFSERIES?$format=json"

    val odata = new Eurostat();
    val list = odata.gitems(Map("url" -> urlList, "list" -> "true"))

    println(list.filter((a) => a.data("DatasetCode").equals("MEI_REAL"))(0).data("DatasetMetadata"))
  }
}
