package sources

import java.util
import java.util.logging.Level

import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.Select
import org.openqa.selenium.{By, WebDriver, WebElement}

import scala.collection.JavaConverters._

/**
 * Created by evka on 14.7.2016.
 */
object DataSourceWebDriverStatic {

  class DataSourceWebDriverFactory {
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

    def refresh(): Unit = driver = new ChromeDriver()

    def getDriver(): WebDriver = {
      if (driver == null) {
        driver = new ChromeDriver();
        return driver
      } else {
        return driver
      }
    }
  }

  def getFactory(): DataSourceWebDriverFactory = {
    new DataSourceWebDriverFactory()
  }

  def arise(func: (String) => By)(driver: WebDriver, xpath: String): util.List[WebElement] = driver.findElements(func(xpath))

  def assign(func: (String) => By)(driver: WebDriver, xpath: String, value: String): Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.sendKeys(value))

  def select(func: (String) => By)(driver: WebDriver, xpath: String, value: String): Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => new Select(element).selectByValue(value))

  def submit(func: (String) => By)(driver: WebDriver, xpath: String): Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.submit())

  def clear(func: (String) => By)(driver: WebDriver, xpath: String): Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.clear())

  def click(func: (String) => By)(driver: WebDriver, xpath: String): Unit =
    driver.findElements(func(xpath)).asScala.foreach((element) => element.click())

  def find(func: (String) => By)(driver: WebDriver, xpath: String): Unit =
    driver.findElements(func(xpath))

  def quit()(driver: WebDriver, xpath: String): Unit = driver.quit()


  implicit class DataSourceWebDriverStaticHelper(driver: WebDriver) {

    /* Hide this long find by elements */
    def xarise(xpath: String): String = arise(By.xpath)(driver, xpath).get(0).getText

    def xassign(xpath: String, value: String): Unit = assign(By.xpath)(driver, xpath, value)

    def xselect(xpath: String, value: String): Unit = select(By.xpath)(driver, xpath, value)

    def xsubmit(xpath: String): Unit = submit(By.xpath)(driver, xpath)

    def xclear(xpath: String): Unit = clear(By.xpath)(driver, xpath)

    def xclick(xpath: String): Unit = click(By.xpath)(driver, xpath)

    def xfind(xpath: String): Unit = find(By.xpath)(driver, xpath)

    def narise(xpath: String): String = arise(By.name)(driver, xpath).get(0).getText

    def nassign(xpath: String, value: String): Unit = assign(By.name)(driver, xpath, value)

    def nselect(xpath: String, value: String): Unit = select(By.name)(driver, xpath, value)

    def nsubmit(xpath: String): Unit = submit(By.name)(driver, xpath)

    def nclear(xpath: String): Unit = clear(By.name)(driver, xpath)

    def nclick(xpath: String): Unit = click(By.name)(driver, xpath)

    def carise(xpath: String): String = arise(By.cssSelector)(driver, xpath).get(0).getText

    def cassign(xpath: String, value: String): Unit = assign(By.cssSelector)(driver, xpath, value)

    def cselect(xpath: String, value: String): Unit = select(By.cssSelector)(driver, xpath, value)

    def csubmit(xpath: String): Unit = submit(By.cssSelector)(driver, xpath)

    def cclear(xpath: String): Unit = clear(By.cssSelector)(driver, xpath)

    def cclick(xpath: String): Unit = click(By.cssSelector)(driver, xpath)

    def ping(): WebDriver = {
      driver.navigate().refresh()
      return driver
    }
  }

}
