/*
 * The MIT License (MIT)
 *
 * Copyright (c) <2013> <Evgeni Kappinen>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package sources

import org.jsoup.nodes.Document

import org.joda.time.DateTime
import scala.collection.mutable
import io.LocalStorage._
import types.PredData


/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 5/9/13
 * Time: 7:20 PM
 *
 *
 * Example:
 *  val mazda = gitem("http://www.nettiauto.com/mazda/323/5340128")
 */

object NettiAutoData extends DataSourceJsoup {

  override val name: String = "NettiAutoData"

  //http://stackoverflow.com/questions/4636610/regular-expression-and-pattern-matching-in-scala
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def matchVaihteisto(x:String): Int = x match  {
    case "Automaatti" => 1
    case "Manuaali" => 2
    case _ => println("Falling to default value for Vaihteisto:" +  x); 3 //default manual
  }


  def matchMittarilukema(x:String): Int = x match {
    case "AJAMATON" => 0
    case _ => x.replace("km", "").replace(" ", "").toInt
  }


  def matchVuosimalli(x:String):Int = x.substring(0, 4).toInt


  def matchMoottori(x:String):Int = (x.replace("l", "").substring(0, 3).toFloat * 100).toInt


  //TODO:rewrite
  def matchKatsastettu(x:String):Int = {

    if (x.toLowerCase.matches("uusi auto")) {
      return DateTime.now().getYear * 12 + DateTime.now().getMonthOfYear
    }

    val date =  x.split("-");

    if (date.size == 2) {
      return date(1).toInt * 12 + date(0).toInt
    } else {
      throw new NumberFormatException("Failed to parse")
    }
  }


  def matchVetotapa(x:String):Int = x match  {
      case "Takaveto" => 1
      case "Etuveto" => 2
      case "Neliveto" => 3
      case _ => println("Falling to default value for Vaihteisto:" +  x); 4 //default manual
  }

  def matchOwner(x:String): Int = x.toLowerCase match {
    case r"yksityinen myyj[a|ä].*" => 0
    case r"autoliike.*" => 1
    case r"myyj[a|ä].*" => 2
    case _ => 3
  }


  def matchPrice(x:String): Int = x.replaceAll("€", "").replaceAll(" ", "").toInt


  def parsePred(key:String, value:String) : (String, Any) = {
    key match {
      case "Vaihteisto" =>  (key -> matchVaihteisto(value))
      case "Mittarilukema" =>  (key -> matchMittarilukema(value))
      case "Vuosimalli" =>  (key -> matchVuosimalli(value))
      case "Moottori" =>  (key -> matchMoottori(value))
      case "Katsastettu" =>  (key -> matchKatsastettu(value))
      case "Vetotapa" =>  (key -> matchVetotapa(value))
      case "Owner" =>  (key -> matchOwner(value))
      case "Price" =>  (key -> matchPrice(value))
      case _ => (key -> value)
    }
  }


  protected def createMap(fetchData: (String) => Document)(head:String, tail:String, idx:Int): Seq[String] = {
    var list = mutable.MutableList[String]()

    println("Fetching map:" + head + idx + tail)
    lazy val urls = fetchData(head + idx + tail).select("a[class=childVifUrl tricky_link")

    for (urlIndx <- 0 to (urls.size() - 1))
      list += urls.get(urlIndx).attr("href")

    list.toSeq
  }


  override def parse(fetchData:(String) => Document)(url:String): PredData = {

    val result = getPredData(NettiAutoData.name, url)
    if (result != null) {
      return result
    }

    println("Fetching:" + url)
    val doc = fetchData(url)


    var values:Map[String, Any] = Map()

    values += parsePred("Price", doc.select("span[itemprop=price]").text())
    values += parsePred("CarInfo", doc.select("p.caption").first().text())
    values += parsePred("Model", doc.select("span[itemprop=model]").first().text())
    values += parsePred("Manufacturer",
                        doc.select("span[itemprop=manufacturer]").select("span[itemprop=name]").first().text())
    values += parsePred("Owner", doc.select("li[class=dcol1]").text())


    /* Fetch car properties */
    val tableElements = doc.select("table[class=mt10 data_table]")
    val carElem = tableElements.select("tr")
    val idxSize = carElem.size() - 1

    for (idx <- 0 to idxSize) {
      val tdElem = carElem.get(idx).select("td")

      values += parsePred(tdElem.get(0).text(), tdElem.get(1).text())
      if (tdElem.size()>3)
        values += parsePred(tdElem.get(3).text(), tdElem.get(4).text())

    }

    writePredData(NettiAutoData.name, PredData(url, DateTime.now().getMillis, List(), values))
  }

  override def parseItems(fetchData:(String) => Document)(opts:Map[String,String]): Seq[PredData] = {
    val fromVal = opts.apply("from").toInt
    val toVal = opts.apply("to").toInt
    val head = opts.apply("head")
    val tail = opts.apply("tail")

    val urls = (fromVal to toVal)
                  .map((pageNum) => createMap(fetchData)(head, tail, pageNum))
                  .reduce((x, y) => (y ++ x))

    urls.map((url) => try {
                        parse(fetchData)(url)
                      } catch {
                        case e:Exception => println("Failed to fetch:" + url); null
                      })
        .filter((data) => data.isInstanceOf[PredData])
  }


  def con_data(rawdata:Seq[types.PredData], list:List[String]) : Seq[types.PredData] = list.size match {
    case 0 => return rawdata
    case _ =>
      val groupName = list(0)
      val group = uniq_id((w) => w.get.toString.trim.toLowerCase)(rawdata, groupName)

      return con_data(
        rawdata.map((predData) => {
          new PredData( predData.source, predData.dtime,
            predData.tags :+ predData.data(groupName).toString,
            predData.data.updated(groupName,
              group.get(predData.data(groupName).toString.toLowerCase).get))
        }), list.filter((aa)=> !aa.equals(groupName)))
  }

  def predata() : Seq[types.PredData] = {
    con_data(all_avail(NettiAutoData.name), List("Sijainti","Model", "Manufacturer"))
  }

}

//object test{
//  import sources.NettiAutoData._
//  val list = gitems(Map("from" -> "1", "to" -> "63",
//    "head" -> "http://www.nettiauto.com/listAdvSearchFindAgent.php?page=",
//    "tail" -> "&sortCol=price&ord=ASC&id=93936333&tb=tmp_find_agent&posted_by=&new=&update=&PN%5B0%5D=adv_search&PL%5B0%5D=advSearch.php%3Fid%3D93936333%26tb%3Dtmp_find_agent&PN%5B1%5D=search_results&PL%5B1%5D=listAdvSearchFindAgent.php%3Fid%3D93936333%26tb%3Dtmp_find_agent"))
//  val rawdata = all_avail(name)
//  val list = List("Sijainti","Model", "Manufacturer")
//  val ii = con_data(rawdata, List("Sijainti","Model", "Manufacturer"))
//
//  gitem("http://www.nettiauto.com/renault/megane/5444381")
//}