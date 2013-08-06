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

import helper.Common._
import types.PredData
import org.joda.time.DateTime


abstract class AirPortSource extends DataSourceWebDriver {


  def iterate(parseUrl : (String, String, String, String) => PredData)
             (fromCities: Seq[String], toCities: Seq[String], fromDate: String, toDate: String): Seq[PredData] = {

    var prices: Seq[PredData] = Seq[PredData]()
    val total = fromCities.size * toCities.size * weekendsWithInPeriod(fromDate, toDate).size
    var count = 0;

    for (from <- fromCities; to <- toCities.reverse; dates <- weekendsWithInPeriod(fromDate, toDate)) {

      printf("Fetching: [%s:%s] dates [%s:%s] = [%d/%d]\n",
        from, to, date2str(dates._1), date2str(dates._2), count, total)
      count += 1

      try {
        prices = prices ++ Seq(parseUrl(from, to, date2str(dates._1), date2str(dates._2)))
      } catch {
        case e: Exception => printf("Failed to fetching: [%s:%s] dates [%s:%s] :%s\n",
          from, to, date2str(dates._1), date2str(dates._2), e.getMessage)
      }
    }
    prices
  }


  def iterateDays(parseUrl : (String, String, String, String) => PredData)
             (fromCities: Seq[String], toCities: Seq[String], days: IndexedSeq [(DateTime, DateTime)]): Seq[PredData] = {

    var prices: Seq[PredData] = Seq[PredData]()
    val total = fromCities.size * toCities.size * days.size
    var count = 0;

    for (from <- fromCities; to <- toCities.reverse; dates <- days) {

      printf("Fetching: [%s:%s] dates [%s:%s] = [%d/%d]\n",
        from, to, date2str(dates._1), date2str(dates._2), count, total)
      count += 1

      try {
        prices = prices ++ Seq(parseUrl(from, to, date2str(dates._1), date2str(dates._2)))
      } catch {
        case e: Exception => printf("Failed to fetching: [%s:%s] dates [%s:%s] :%s\n",
          from, to, date2str(dates._1), date2str(dates._2), e.getMessage)
      }
    }
    prices
  }



  val dest = Map(
    "Barcelona" -> "BCN",
    "Milano"    -> "MIL",
    "Pariisi"    -> None,
    "Rooma"    -> None,
    "Oslo"    -> None,
    "Dubrovnik"    -> None,
    "Praha"    -> None,
    "München"    -> None,
    "Berliini"    -> None,
    "Kööpenhamina"    -> None,
    "Lontoo"    -> None,
    "Reykjavik"    -> None,
    "Wien"    -> None,
    "Kiova"    -> None,
    "Madrid"    -> None,
    "Bukarest"    -> None,
    "Hampuri"    -> None,
    "Minsk"    -> None,
    "Varsova"    -> None,
    "Minsk"    -> None,
    "Belgrade"    -> None,
    "Krakova"    -> None,
    "Barcelona"    -> None,
    "Sofia"    -> None,
    "Dnipropetrovsk"    -> None,
    "Napoli"    -> None,
    "Odessa"    -> None,
    "Torino"    -> None,
    "Zagreb"    -> None,
    "Marseille"    -> None,
    "Riika"    -> None,
    "Lvov"    -> None,
    "Ateena"    -> None,
//    "Salamanca"    -> None,
    "Tukholma"    -> None,
    "Krakova"    -> None,
    "Valencia"    -> None,
    "AMSTERDAM"    -> None,
    "Kishinev"    -> None,
    "Geneve"    -> None,
    "Tbilisi"    -> None,
    "Baku"    -> None,
    "Frankfurt"    -> None,
    "Wroclaw"    -> None,
    "Glasgow"    -> None,
//    "Zaragoza"    -> None,
    "Rotterdam"    -> None,
    "Vilna"    -> None,
    "Düsseldorf"    -> None,
    "Lissabon"    -> None,
    "Malaga"    -> None,
    "Bremen"    -> None,
//    "Sheffield"    -> None,
    "Hannover"    -> None,
    "Zürich"    -> None,
    "Palma De Mallorca"    -> None,
    "Bryssel"    -> None,
    "Dublin"    -> None,
    "Nizza"    -> None,
    "Las Palmas"    -> None,
    "Izmir"    -> None,
    "Köln"    -> None,
    "Edinburgh"    -> None,
    "Alicante"    -> None,
    "Teneriffa"    -> None,
    "Budapest"    -> None,
    "Lyon"    -> None,
    "Toulouse"    -> None,
    "Stuttgart"    -> None,
    "Charleroi"    -> None,
//    "Catania"    -> None,
    "Oporto"    -> None,
    "Bologna"    -> None,
    "Bergen"    -> None,
    "Napoli"    -> None,
//    "Faro (Algarve)"    -> None,
    "Ibiza"    -> None,
    "Larnaca"    -> None,
//    "Kreeta Heraklion"    -> None,
    "Palermo"    -> None,
    "Lanzarote"    -> None
  )

  def gitem(criteria: String): PredData = ???

  def gitems(criteria: Map[String, String]): Seq[PredData] = ???
}





//
//    def parseUrl2(flightFrom: String, flightTo: String,
//                 flightFromDate: String, flightToDate: String): PredData = {
//
//      val url = "Flight from " + flightFrom + " to " + flightTo + " during: " + flightFromDate + ":" + flightToDate
//
//      val result = LocalStorage.getPredData(AirPortSource.name, url)
//      if (result != null) {
//        return result
//      }
//
//      val driver: HtmlUnitDriver = new HtmlUnitDriver
////      driver.setJavascriptEnabled(true)
//      driver.get("http://www.seat24.fi/")
//
//      val asValues = Map[String, String](
//                "//*[@id=\"top_content_fromCity\"]" -> flightFrom,
//                "//*[@id=\"top_content_toCity\"]" -> flightTo,
//                "//*[@id=\"outDateId\"]" -> flightFromDate,
//                "//*[@id=\"returnDateId\"]" -> flightToDate)
//
//      asValues.map((value) => assign(driver, value._1, value._2))
//
//      new Select(driver.findElement(By.xpath("//*[@id=\"depDateTimeOfDay\"]"))).selectByValue("AFTERNOON")
//      driver.findElement(By.xpath("//*[@id=\"airSubmitButtonId\"]")).submit()
//
//      val price = driver.findElement(By.xpath("//*[@id=\"tg1\"]/div/div/div/div/div[2]/div[1]")).getText
//
//      var values: Map[String, Any] = Map("FromCity" -> flightFrom, "ToCity" -> flightTo,
//                                         "FromDate" -> flightFromDate,"ToDate" -> flightToDate,
//                                         "Price" -> priceConv(price))
//
//      driver.close()
//      Thread.sleep((10000*math.random + 5000*math.random).toLong)
//
//      LocalStorage.writePredData(AirPortSource.name, PredData(url, DateTime.now().getMillis, List(), values))
//    }