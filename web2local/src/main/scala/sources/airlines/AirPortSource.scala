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

package sources.airlines

import common.Utils._
import types.DataItem
import org.joda.time.DateTime
import sources.DataSourceWebDriver


abstract class AirPortSource extends DataSourceWebDriver {


  def iterate(parseUrl : (String, String, String, String) => DataItem)
             (fromCities: Seq[String], toCities: Seq[String], fromDate: String, toDate: String): Seq[DataItem] = {

    var prices: Seq[DataItem] = Seq[DataItem]()
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


  def iterateDays(parseUrl : (String, String, String, String) => DataItem)
             (fromCities: Seq[String], toCities: Seq[String], days: IndexedSeq [(DateTime, DateTime)]): Seq[DataItem] = {

    var prices: Seq[DataItem] = Seq[DataItem]()
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

  def gitem(criteria: String): DataItem = ???

  def gitems(criteria: Map[String, String]): Seq[DataItem] = ???
}