package analytics.cases

import scala.math._
import analytics.Regression._
import analytics.Plots._
import io.LocalStorage._
import sources.misc.NettiAutoData
import NettiAutoData._

/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 8/9/13
 * Time: 6:56 PM
 
The MIT License (MIT)

Copyright (c) <2013> <Evgeni Kappinen>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 */
class NettiAutoModel {

  def run() = {

    val cars = all_avail()
    val data = cars.filter((a) => a("Model").equals("Tiida"))

    //    val data = all_avail().filter((pre) => pre("Manufacturer").equals("Opel")).filter((pre) => pre("Vuosimalli").toString.toInt > 1995).filter((pre) => pre("Model").equals("Astra"))
//    val data = all_avail().filter((pre) => pre("Model").equals("Focus")).filter((pre) => pre("Vuosimalli").toString.toInt > 1995)
//    data(1)

    val group = uniq_id((w) => w.get.toString.trim.toLowerCase)(data, "Vaihteisto")
    //    val group = groupByCount((w) => w.get.toString.trim.toLowerCase)(data, "Model")
    //    groupByCount((w) => w.get.toString.trim.toLowerCase)(data, "Owner")


    val price = data_as[Double]("Price", data)("Price").toArray
    val year = data_as[Double]("Vuosimalli", data)("Vuosimalli").toArray
    val kilom = data_as[Double]("Mittarilukema", data)("Mittarilukema").toArray
    val mottor = data_as[Double]("Moottori", data)("Moottori").toArray
    val vaiht = data_as[Double]("Vaihteisto", data)("Vaihteisto").toArray
    val owner = data_as[Double]("Owner", data)("Owner").toArray


    val year2 = year.map((a) => scala.math.log(a))
    val kilom2 = kilom.map((a) => scala.math.log(a))


    //    import analytics.Regression._
    //    val za = regress(price, multiply(year2,kilom2))
    //    val za = regress(price, multiply(year,kilom))
    //    val za = regress(price, kilom2.map((z) => Array(z)))
    //    val za = regress(price, (year2 zip kilom2).map((z) => Array(z._1, z._2)))


    //    ------------------------------------------------------
    val price2 = price.map((a) => scala.math.log(a))
    plot(kilom, price)
    val za2 = regress(price2, kilom2)   // 1846.1823711489349
    plota(kilom, kilom.map((a)  => 34500.85182202326-2288.3884505756278*scala.math.log(a)))
    plota(kilom, kilom.map((a)  => scala.math.exp(9.258941158778468-2.766654763789916E-6*a)))
    plota(kilom, kilom2.map((a)  => scala.math.exp(12.554985491628742-0.31123917217819297*a)))
    //    ------------------------------------------------------

    plot(year, price)
    val za3 = regress(price2, year2)
    plota(year, year.map((a)  => -1668705.9033436186+836.1355241716584*a))
    plota(year, year2.map((a)  => -1.2733338303853802E7 + 1675709.0088116243*a))
    plota(year, year2.map((a)  => scala.math.exp(-2011.1260909286987+265.6766554337401*a)))
    plota(price2, res2array(za3, year2))

    val ii = year2.map((a)  => scala.math.exp(-2011.1260909286987+265.6766554337401*a))
    year2.map((a)  => scala.math.exp(-2011.1260909286987+265.6766554337401*a))


    hist(kilom)

    //    regression.estimateRegressionStandardError()
    (year.zip(kilom)).map((a) => a._1 + a._2)
    plot((year.zip(kilom)).map((a) => Math.log(a._1 * 1000) + Math.log(a._2/1000)), price)
    plot(year, price)
    hist(kilom.map((a)=> a - 2005))
    hist(year)


    data(0)
    //    BEST FIT
    val all = year.zipWithIndex.map((a)=> Array(log(a._1), log(kilom(a._2)), log(mottor(a._2)), vaiht(a._2) + 1, owner(a._2) + 1))
    val za4 = regress(price, all)
    regression.estimateRegressionStandardError()

    data.filter((a) => a("Owner").toString.equals("1"))
    data(0)
    res2array(za4, Array(Array[Double](log(2008),log(60000),log(160),log(2),log(1)))).map((a) => exp(a))
    res2array(za4, Array(Array[Double](log(2008),log(60000),log(160),log(2),log(1)))).map((a) => a)



    plot(price, res2array(za4, all).map((a) => exp(a)))
    plota(price, price)
    data.filter((a) => a("Price") == price.min).size


  }

}
