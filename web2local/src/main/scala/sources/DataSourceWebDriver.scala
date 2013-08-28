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

import types.PredData
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.{WebDriver, By}
import io.LocalStorage
import org.openqa.selenium.support.ui.Select
import java.util.logging.Level


trait DataSourceWebDriver extends DataSource {

  /**
   *
   * Goal: Create map of items, then fetch item, then parse item into usable form (ItemData type)
   *
   * TODO: Cache
   * TODO: store values to database = ??
   * TODO: Move to xpath
   */

  java.util.logging.Logger.getLogger("org.openqa.selenium.htmlunit").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("com.gargoylesoftware.htmlunit").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("org.apache.commons.httpclient").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("org.apache.http").setLevel(Level.OFF)
  java.util.logging.Logger.getLogger("").setLevel(Level.OFF)



  def arise(driver: WebDriver, xpath:String) : String = driver.findElement(By.xpath(xpath)).getText
  def narise(driver: WebDriver, xpath:String) : String = driver.findElement(By.name(xpath)).getText
  def assign(driver: WebDriver, xpath:String, value:String) : Unit = driver.findElement(By.xpath(xpath)).sendKeys(value)
  def nassign(driver: WebDriver, xpath:String, value:String) : Unit = driver.findElement(By.name(xpath)).sendKeys(value)
  def select(driver: WebDriver, xpath:String, value:String) : Unit = new Select(driver.findElement(By.xpath(xpath))).selectByValue(value)
  def sumbit(driver: WebDriver, xpath:String) : Unit = driver.findElement(By.xpath(xpath)).submit()
  def getDriver() : WebDriver = new HtmlUnitDriver()
  def price2Double(price:String) : Double = price.trim.replaceAll(" ", "").replaceAll("€","").replaceAll(",",".").toDouble
  def printData(data:Seq[PredData]) = data.sortWith((a,b) => (price2Double(a.data("Price").toString) < price2Double(b.data("Price").toString))).
    map((ap) => println(ap.source + " and price:" + ap.data("Price")))

  def manageUrl(func:(WebDriver) => PredData)
               (preFunc:Seq[(WebDriver) => Unit])
               (moduleName:String, id: String, url: String,
                assignMap:Map[String,String],
                selectMap:Map[String,String],
                sumbitMap:Seq[String]): PredData = {

    val result = LocalStorage.getPredData(moduleName, id)
    if (result != null) {
      return result
    }

    val driver: WebDriver = getDriver()

    driver.get(url)
    preFunc.map((a) => a(driver))

    assignMap.map((variable) => assign(driver, variable._1, variable._2))
    selectMap.map((variable) => select(driver, variable._1, variable._2))
    sumbitMap.map((variable) => sumbit(driver, variable))

    val predData = func(driver)

//    driver.close()

    LocalStorage.writePredData(moduleName, predData)
  }

}
