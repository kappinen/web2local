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

import types.DataItem
import org.openqa.selenium.{WebElement, WebDriver, By}
import io.LocalStorage
import org.openqa.selenium.support.ui.Select
import java.util.logging.Level
import java.util
import scala.collection.JavaConverters._
import org.openqa.selenium.chrome.{ChromeOptions, ChromeDriver}

abstract class DataSourceWebDriver extends DataSource {
  var driver: WebDriver = null;

  {
    System.setProperty("webdriver.chrome.driver", "/home/home/Dropbox/web/files/chromedriver")

    java.util.logging.Logger.getLogger("org.openqa.selenium.htmlunit").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("com.gargoylesoftware.htmlunit").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("org.apache.commons.httpclient").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("org.apache.http").setLevel(Level.OFF)
    java.util.logging.Logger.getLogger("").setLevel(Level.OFF)
  }

  def refresh() : Unit = driver = new ChromeDriver()
  /**
   *
   * Goal: Create map of items, then fetch item, then parse item into usable form (ItemData type)
   *
   * TODO: Cache
   * TODO: store values to database = ??
   * TODO: Move to xpath
   */

  def incognito(): WebDriver = {

    val chromeOpts = new ChromeOptions();
    chromeOpts.addArguments("--incognito");
    driver = new ChromeDriver(chromeOpts);

    return driver
  }

  //new HtmlUnitDriver()
  def getDriver(): WebDriver = {
    if (driver == null) {
      driver = new ChromeDriver();
      return driver
    } else {
      return driver
    }
  }

  def arise(func:(String) => By)(driver: WebDriver, xpath:String) : util.List[WebElement] = driver.findElements(func(xpath))

  def assign(func:(String) => By)(driver: WebDriver, xpath:String, value:String) : Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.sendKeys(value))

  def select(func:(String) => By)(driver: WebDriver, xpath:String, value:String) : Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => new Select(element).selectByValue(value))

  def submit(func:(String) => By)(driver: WebDriver, xpath:String) : Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.submit())

  def clear(func:(String) => By)(driver: WebDriver, xpath:String) : Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.clear())

  def click(func:(String) => By)(driver: WebDriver, xpath:String) : Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.click())

  def quit()(driver: WebDriver, xpath:String) : Unit = driver.quit()


  /* Hide this long find by elements */
  def xarise(driver: WebDriver, xpath:String) : String = arise(By.xpath)(driver, xpath).get(0).getText
  def xassign(driver: WebDriver, xpath:String, value:String) : Unit = assign(By.xpath)(driver, xpath, value)
  def xselect(driver: WebDriver, xpath:String, value:String) : Unit = select(By.xpath)(driver, xpath, value)
  def xsubmit(driver: WebDriver, xpath:String) : Unit = submit(By.xpath)(driver, xpath)
  def xclear(driver: WebDriver, xpath:String) : Unit = clear(By.xpath)(driver, xpath)
  def xclick(driver: WebDriver, xpath:String) : Unit = click(By.xpath)(driver, xpath)

  def narise(driver: WebDriver, xpath:String) : String = arise(By.name)(driver, xpath).get(0).getText
  def nassign(driver: WebDriver, xpath:String, value:String) : Unit = assign(By.name)(driver, xpath, value)
  def nselect(driver: WebDriver, xpath:String, value:String) : Unit = select(By.name)(driver, xpath, value)
  def nsubmit(driver: WebDriver, xpath:String) : Unit = submit(By.name)(driver, xpath)
  def nclear(driver: WebDriver, xpath:String) : Unit = clear(By.name)(driver, xpath)
  def nclick(driver: WebDriver, xpath:String) : Unit = click(By.name)(driver, xpath)

  def carise(driver: WebDriver, xpath:String) : String = arise(By.cssSelector)(driver, xpath).get(0).getText
  def cassign(driver: WebDriver, xpath:String, value:String) : Unit = assign(By.cssSelector)(driver, xpath, value)
  def cselect(driver: WebDriver, xpath:String, value:String) : Unit = select(By.cssSelector)(driver, xpath, value)
  def csubmit(driver: WebDriver, xpath:String) : Unit = submit(By.cssSelector)(driver, xpath)
  def cclear(driver: WebDriver, xpath:String) : Unit = clear(By.cssSelector)(driver, xpath)
  def cclick(driver: WebDriver, xpath:String) : Unit = click(By.cssSelector)(driver, xpath)



  def printData(data:Seq[DataItem]) = data.sortWith((a,b) => (price2Double(a.data("Price").toString) < price2Double(b.data("Price").toString))).
    map((ap) => println(ap.source + " and price:" + ap.data("Price")))

  def manageUrl(func:(WebDriver) => DataItem)
               (preFunc:Seq[(WebDriver) => Unit])
               (moduleName:String, id: String, url: String,
                assignMap:Map[String,String],
                selectMap:Map[String,String],
                submitMap:Seq[String]): DataItem = {

    val result = LocalStorage.getPredData(moduleName, id)
    if (result != null) {
      return result
    }
    //FIXME needs to be here?
    val driver: WebDriver = getDriver()

    driver.get(url)
    preFunc.map((a) => a(driver))

    assignMap.map((variable) => xassign(driver, variable._1, variable._2))
    selectMap.map((variable) => xselect(driver, variable._1, variable._2))
    submitMap.map((variable) => xsubmit(driver, variable))

    val predData = func(driver)

    LocalStorage.writePredData(moduleName, predData)
  }

}
