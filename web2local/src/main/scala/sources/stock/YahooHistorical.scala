package sources.stock

import sources.DataSourceSimple
import io.LocalStorage._
import common.Utils._
import org.joda.time.DateTime
import sources.stock.GoogleHistorical._
import types.DataItem

import scala.reflect.io.File

/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 8/26/13
 * Time: 9:21 PM
 */
object YahooHistorical extends DataSourceSimple {
  val name: String = "YahooHistorical"



  protected def parse(fetchData: (String) => String)(url: String): DataItem =
    csvFromString(srcParser)(url, fetchData(url), ",").head

  protected def parseItems(fetchData: (String) => String)(opts: Map[String, String]): Seq[DataItem] =
    csvFromString(srcParser)(opts("url"), fetchData(opts("url")), ",")


  def yahooUrlFormatter(indx: String, fromDate: String = "1990-01-01", toDate: String = date2str(new DateTime())): String = {
    val startDate = str2date(fromDate)
    val endDate = str2date(toDate)

    val header = "http://ichart.finance.yahoo.com/table.csv?s="
    val toDateString = "&d=" + (endDate.getMonthOfYear - 1) + "&e=" + endDate.getDayOfMonth + "&f=" + endDate.getYear

    val fromDateString = "&g=d&a=" + (startDate.getMonthOfYear - 1) + "&b=" + startDate.getDayOfMonth + "&c=" + startDate.getYear

    val tail = "&ignore=.csv"

    header + indx + toDateString + fromDateString + tail
  }


  def loadLatest(symbol: String, startDate: String = "1990-01-01", endDate: String = date2str(new DateTime())): Seq[DataItem] = {
    csvFromString(
      srcYahooFinance)(symbol,
        ftext(yahooUrlFormatter(symbol, startDate, endDate)).replace("\uFEFF", ""),
        ",")
  }

  def test(): Unit = {

    val oil = loadLatest("OIL", "2001-01-01")

    csvFromString(srcYahooFinance)("test",File("/tmp/web2local/a84dd14747be278d4edb67739839448.cache").slurp(), ",")
  }

}