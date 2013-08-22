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

package helper

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

object Common {
  val pathResources:String = "resources"
  val pathRDirectory:String = "r-files"
  // Source: http://stackoverflow.com/questions/3073677/implicit-conversion-to-runnable
  def thread[F](f: => F) = (new Thread( new Runnable() { def run() { f } } )).start


  def threadWithTimer[F](timer: => Unit = Thread.sleep(1000))(f: => F) = {
    timer
    (new Thread( new Runnable() { def run() { f } } )).start
  }

  def str2cents(value:String) : Int = (value.trim.replaceAll(",", ".").toDouble * 100).toInt

  def str2date(date: String) : DateTime = DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(date)
  def date2str(date: DateTime) : String =  DateTimeFormat.forPattern("yyyy-MM-dd").print(date)

  def epoc2str(date:Long) : String = date2str(new DateTime(date))

  def bf2console(data: String) = data.split("\n").map((line) => printf("%s\n", line))


  def weekendsWithInPeriod(startD: String, endD: String): IndexedSeq [(DateTime, DateTime)] = {
    val skip = org.joda.time.DateTimeConstants.FRIDAY - str2date(startD).getDayOfWeek
    val firstFriday = str2date(startD).plusDays(skip)

    val weeks = (str2date(endD).getMillis - str2date(startD).getMillis) / (1000 * 60 * 60 * 24 * 7)

    (0 to weeks.toInt).map((week) => (firstFriday.plusWeeks(week), firstFriday.plusWeeks(week).plusDays(2)))
  }

}
