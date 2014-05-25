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
import io.LocalStorage._
import types.DataItem
import org.joda.time.DateTime
import analytics.Plots._
import analytics.Regression._
import types.DataItem

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

      retSeq =  DataItem(name, 0L, null, map.map((a) => ((a._1:String) -> (a._2.toString:Any))).toMap) +: retSeq
    }

    if (opts.getOrElse[String]("list", "false").toBoolean) {
      retSeq.foreach((a) => println(a.data("DatasetCode") + ":" + a.data("DatasetTitle")))
    }

    retSeq
  }


  override protected def parse(fetchData: (String) => String)(url: String): DataItem =
    parseItems(fetchData)(Map("url" -> url)).head


  private def example_odata_merge_mei_real() = {
//    import sources.stats.Eurostat

    val urlData = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/REFSERIES?$format=json"
    val urlData2 = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/MEI_REAL?$format=json"

    val odata = new Eurostat();

    val data = odata.gitems(Map("url" -> urlData2))


    val usdEuro = csv(defParserOanda)("./resources/currency/usd-euro_monthly.csv", ",").
      map((a) => a ++ Map("TIME" -> (a("End Date").substring(0,4) +"M" + a("End Date").substring(5,7).toInt)))

    val marketData=DataItemUtil.mergeBy((a:DataItem,b:DataItem) => a("TIME").
      equals(b("TIME")))(data.filter((a) => a("FREQUENCY").equals("M")).filter((a) => a("SUBJECT").equals("SLRTTO01")).filter((a) => a("LOCATION").startsWith("USA")),
      data.filter((a) => a("FREQUENCY").equals("M")).filter((a) => a("SUBJECT").equals("SLRTTO01")).filter((a) => a("LOCATION").startsWith("FIN")),
      data.filter((a) => a("FREQUENCY").equals("M")).filter((a) => a("SUBJECT").equals("SLRTTO01")).filter((a) => a("LOCATION").startsWith("RUS")),
      usdEuro)


    import analytics.Regression._
    corr(diff(marketData(0).reverse.map((a) => math.log(a("Value").toDouble))), diff(marketData(1).reverse.map((a) => math.log(a("Value").toDouble))))
    corr(diff(marketData(0).reverse.map((a) => math.log(a("Value").toDouble))), diff(marketData(2).reverse.map((a) => math.log(a("Value").toDouble))))
    corr(diff(marketData(1).reverse.map((a) => math.log(a("Value").toDouble))), diff(marketData(2).reverse.map((a) => math.log(a("Value").toDouble))))



    plotRefresh()
    plot(diff(marketData(3).reverse.map((a) => math.log(a("USD/EUR").toDouble))),
         diff(marketData(0).reverse.map((a) => math.log(a("Value").toDouble))))

    plotx(diff(marketData(1).reverse.map((a) => math.log(a("Value").toDouble))))
    plotx(diff(marketData(2).reverse.map((a) => math.log(a("Value").toDouble))))
    plotx(diff(marketData(3).reverse.map((a) => math.log(a("USD/EUR").toDouble))))


    /** Fetch dataset codes */
    val urlList = "http://stats.oecd.org/OECDStatWCF_OData/OData.svc/GetDatasets?$format=json"
    val list = odata.gitems(Map("url" -> urlList, "list" -> "true"))
    println(list.filter((a) => a("DatasetCode").equals("MEI_REAL"))(0)("DatasetMetadata"))

  }

}
