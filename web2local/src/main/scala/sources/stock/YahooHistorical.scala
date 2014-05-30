package sources.stock

import sources.DataSourceSimple
import io.LocalStorage._
import common.Utils._
import org.joda.time.DateTime
import types.DataItem

/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 8/26/13
 * Time: 9:21 PM
 */
object YahooHistorical extends DataSourceSimple {
  val name: String = "YahooHistorical"

  val urls:Map[String, String] =
    Map("Randgold Resources Limited" -> "GOLD")


  protected def parse(fetchData: (String) => String)(url: String): DataItem =
    csvFromString(srcParser)(url, fetchData(url), ",").head

  protected def parseItems(fetchData: (String) => String)(opts: Map[String, String]): Seq[DataItem] =
    csvFromString(srcParser)(opts("url"), fetchData(opts("url")), ",")


  def buildUrl(indx: String, fromDate: String, toDate: String): String = {
    //    val indx = "GOLD"
    val startDate = str2date(fromDate)
    val endDate = str2date(toDate)



    val header = "http://ichart.finance.yahoo.com/table.csv?s="
    val toDateString = "&d=" + (endDate.getMonthOfYear - 1) + "&e=" + endDate.getDayOfMonth + "&f=" + endDate.getYear
//    val toDateString = DateTimeFormat.forPattern("'&d='MM'&e='dd'&f='yyyy").print(endDate)
    val fromDateString = "&g=d&a=" + (startDate.getMonthOfYear - 1) + "&b=" + startDate.getDayOfMonth + "&c=" + startDate.getYear
//    val fromDateString = DateTimeFormat.forPattern("'&g=d&a='MM'&b='dd'&c='yyyy").print(startDate)
    val tail = "&ignore=.csv"

    header + indx + toDateString + fromDateString + tail
  }

  def run() = {
    urls.map((url) =>
      csvFromString(srcParser)(url._1, ftext(url._2), ","))

//    import stock.YahooHistorical._

    val url2 = buildUrl("GOLD", "2013-01-01", date2str(new DateTime))
    val gold = csvFromString(srcParser)(url2, ftext(url2), ",")


    val url = buildUrl("^OMXH25", "2013-01-01", date2str(new DateTime))
    val omx = csvFromString(srcParser)(url, ftext(url), ",")


    omx.filter((a) => a("Date").equals("2013-08-29"))


    omx.find((a) => a("Date").equals("2013-08-29")).get
    gold.find((a) => a("Date").equals("2013-08-29")).get


    def mergeBy(a:DataItem) : Any = a("Date")
    val ii = omx.map((a) => mergeBy(a))

    val leading = omx
    val process = Seq(gold)




    def hasNull(a:Seq[DataItem]):Boolean = !a.map((zz) => zz != null).reduce((d,e) => d && e)
    val zz = gold.map((a) => Seq(a, omx.find((data) => mergeBy(data).equals(mergeBy(a))).getOrElse(null)))
    val di = zz.filterNot((a) => hasNull(a))



    val res = leading.map((leader) => Seq(leader) ++ process.map((follower) => follower.find((data) =>
                            mergeBy(data).equals(mergeBy(leader))).getOrElse(null)))

  }

}