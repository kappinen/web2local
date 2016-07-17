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

package common

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import sys.process._

object Utils {
  val pathResources: String = "resources"
  val pathRDirectory: String = "r-files"

  val CALENDAR_EUROPE_FORMAT = "yyyy-MM-dd"
  val CALENDAR_US_FORMAT = "MM/dd/yyyy"

  type Seq[+A] = scala.collection.Seq[A]

  // Source: http://stackoverflow.com/questions/3073677/implicit-conversion-to-runnable
  def thread[F](f: => F) = (new Thread(new Runnable() {
    def run() {
      f
    }
  })).start


  def threadWithTimer[F](timer: => Unit = Thread.sleep(1000))(f: => F) = {
    timer
    (new Thread(new Runnable() {
      def run() {
        f
      }
    })).start
  }

  def str2cents(value: String): Int = (value.trim.replaceAll(",", ".").toDouble * 100).toInt

  def str2date(date: String, format: String = "yyyy-MM-dd"): DateTime = string2date(format, date)

  def strdate2num(date: String, format: String = "yyyy-MM-dd"): Double = {
    val dateValue = string2date(format, date)
    dateValue.year().get() * 10000l + dateValue.monthOfYear().get() * 100l + dateValue.dayOfMonth().get()
  }

  def date2str(date: DateTime, format: String = "yyyy-MM-dd"): String = date2string(format, date)

  private def string2date(format: String, date: String): DateTime = DateTimeFormat.forPattern(format).parseDateTime(date)

  private def date2string(format: String, date: DateTime): String = DateTimeFormat.forPattern(format).print(date)

  def epoc2str(date: Long): String = date2str(new DateTime(date))

  def bf2console(data: String) = data.split("\n").map((line) => printf("%s\n", line))

  def weekendsWithInPeriod(startD: String, endD: String): IndexedSeq[(DateTime, DateTime)] = {
    val skip = org.joda.time.DateTimeConstants.FRIDAY - str2date(startD).getDayOfWeek
    val firstFriday = str2date(startD).plusDays(skip)

    val weeks = (str2date(endD).getMillis - str2date(startD).getMillis) / (1000 * 60 * 60 * 24 * 7)

    (0 to weeks.toInt).map((week) => (firstFriday.plusWeeks(week), firstFriday.plusWeeks(week).plusDays(2)))
  }

  def isBlank(value: String): Boolean = {
    value == null || value.trim.isEmpty
  }

  def isNotBlank(value: String): Boolean = !isBlank(value)

  //http://rosettacode.org/wiki/Determine_if_a_string_is_numeric
  private def throwsNumberFormatException(f: => Any): Boolean = {
    try {
      f; false
    } catch {
      case e: NumberFormatException => true
    }
  }

  def isNumeric(str: String): Boolean = {
    if (isBlank(str)) {
      return false
    }
    !throwsNumberFormatException(toDouble(str))
  }

  def isNotNumeric(str: String): Boolean = !isNumeric(str)

  def toDouble[T](str: T): Double = str match {
    case _: Double => str.asInstanceOf[Double]
    case _: Int => Int.int2double(str.asInstanceOf[Int])
    case _: Long => Long.long2double(str.asInstanceOf[Long])
    case _: String => str.toString.replaceAll(" ", "").replaceAll(",", ".").toDouble
    case _ => str.toString.replaceAll(" ", "").replaceAll(",", ".").toDouble
  }


  def shutDownNow() = "shutdown now" !
}
