package analytics

import java.awt.{Color, BorderLayout, Dimension, GraphicsEnvironment}
import java.text.{DecimalFormat, SimpleDateFormat}
import javax.swing.{JFrame, JPanel}

import common.Utils._
import org.jfree.chart.axis.{DateAxis, NumberAxis}
import org.jfree.chart.labels.StandardXYToolTipGenerator
import org.jfree.chart.plot.{PlotOrientation, CombinedDomainXYPlot, XYPlot}
import org.jfree.chart.renderer.xy._
import org.jfree.chart.urls.StandardXYURLGenerator
import org.jfree.chart.{StandardChartTheme, ChartFactory, JFreeChart, ChartPanel}
import org.jfree.data.time.{FixedMillisecond, TimeSeriesCollection, TimeSeries}
import org.jfree.data.time.ohlc.{OHLCSeriesCollection, OHLCSeries}
import org.jfree.data.xy.XYDataset
import types.DataItem

import scala.collection.mutable.ListBuffer

//import scala.collection.immutable.ListMap
//import scala.collection._

import scala.collection.mutable

/**
 * Created by home on 29.8.2016.
 */
class PlotBatchV2(title: String = "") extends JPanel {
  val data: ListBuffer[(String, Seq[DataItem])] = ListBuffer()
  val subData: ListBuffer[(String, Seq[DataItem])] = ListBuffer()
  val frame: JFrame = new JFrame("Plot")

  def createChart(chartTitle: String): JFreeChart = {

    val dateAxis: DateAxis = new DateAxis("Date")
    dateAxis.setDateFormatOverride(new SimpleDateFormat("dd.MM"))

    dateAxis.setLowerMargin(0.02)
    dateAxis.setUpperMargin(0.02)

    val chart: JFreeChart = createTimeSeriesChart("", "Date", "Series", null, true, true, false)

    val plot: XYPlot = new XYPlot(null, new DateAxis("Date"), new NumberAxis("Series"), null)
    data.zipWithIndex.foreach(dat => {
      val volumeDataset: TimeSeriesCollection = new TimeSeriesCollection()

      val volumeSeries: TimeSeries = new TimeSeries(dat._1._2(0).source + ":" + dat._1._1 + ":" + dat._2)
      val volumeAxis: NumberAxis = new NumberAxis(dat._1._2(0).source + ":" + dat._1._1 + ":" + dat._2)
      volumeAxis.setNumberFormatOverride(new DecimalFormat("0"))

      dat._1._2.foreach(dataItem => {
        val date = new FixedMillisecond(str2date(dataItem("Date")).toDate)
        volumeSeries.add(date, dataItem.toDouble(dat._1._1));
      })

      println("Added size:" + dat._1._2.size + " tag:" + dat._1._1 + " total:" + data.size)
      volumeDataset.addSeries(volumeSeries)
      plot.setDataset(dat._2 + 1, volumeDataset)
      plot.setRenderer(dat._2 + 1, new StandardXYItemRenderer())
    })

    chart.getXYPlot.asInstanceOf[CombinedDomainXYPlot].add(plot, 3)

    if (!subData.isEmpty) {

      val subPlot: XYPlot = new XYPlot(null, new DateAxis("Date"), new NumberAxis("Series"), null)
      subData.zipWithIndex.foreach(dat => {
        val volumeDataset: TimeSeriesCollection = new TimeSeriesCollection()

        val volumeSeries: TimeSeries = new TimeSeries(dat._1._2(0).source + ":" + dat._1._1 + ":" + dat._2)
        val volumeAxis: NumberAxis = new NumberAxis(dat._1._2(0).source + ":" + dat._1._1 + ":" + dat._2)
        volumeAxis.setNumberFormatOverride(new DecimalFormat("0"))

        dat._1._2.foreach(dataItem => {
          val date = new FixedMillisecond(str2date(dataItem("Date")).toDate)
          volumeSeries.add(date, dataItem.toDouble(dat._1._1));
        })

        println("Added size:" + dat._1._2.size + " tag:" + dat._1._1 + " total:" + data.size)
        volumeDataset.addSeries(volumeSeries)
        subPlot.setDataset(dat._2 + 1, volumeDataset)
        subPlot.setRenderer(dat._2 + 1, new StandardXYItemRenderer())
      })
      chart.getXYPlot.asInstanceOf[CombinedDomainXYPlot].add(subPlot, 1)

    }

    chart.getXYPlot.setBackgroundPaint(Color.lightGray)
    chart.getXYPlot.setDomainGridlinePaint(Color.white);
    chart.getXYPlot.setRangeGridlinePaint(Color.white);


    //    chart.removeLegend()
    chart
  }

  def addSeries(items: Seq[DataItem], dataName: String = "Volume"): PlotBatchV2 = {
    data += ((dataName, items))
    this
  }
  def addSubSeries(items: Seq[DataItem], dataName: String = "Volume"): PlotBatchV2 = {
    subData += ((dataName, items))
    this
  }

  //  http://alvinalexander.com/scala/how-add-elements-to-a-list-in-scala-listbuffer-immutable
  def plot(): JFrame = {

    val chartPanel: ChartPanel = new ChartPanel(createChart(title));

    val devices = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices
    chartPanel.setPreferredSize(new Dimension(devices(0).getDisplayMode.getWidth, devices(0).getDisplayMode.getHeight - 70));

    chartPanel.setMouseZoomable(true)
    chartPanel.setMouseWheelEnabled(true)
    add(chartPanel, BorderLayout.CENTER)

    frame.setContentPane(this)
    frame.pack();
    frame.setVisible(true);
    frame
  }

  def clear() = {
    frame.removeAll()
    frame.invalidate()
    frame.repaint()
  }


  def createTimeSeriesChart(title: String,
                            timeAxisLabel: String,
                            valueAxisLabel: String,
                            dataset: XYDataset,
                            legend: Boolean,
                            tooltips: Boolean,
                            urls: Boolean): JFreeChart = {
    val timeAxis: DateAxis = new DateAxis(timeAxisLabel)
    timeAxis.setLowerMargin(0.02D)
    timeAxis.setUpperMargin(0.02D)

    val valueAxis: NumberAxis = new NumberAxis(valueAxisLabel)
    valueAxis.setAutoRangeIncludesZero(false)

//    val plot: XYPlot = new XYPlot(dataset, timeAxis, valueAxis, null)
    val plot: CombinedDomainXYPlot = new CombinedDomainXYPlot(timeAxis)
    plot.setOrientation(PlotOrientation.VERTICAL);

    plot.setGap(3.0)
    var toolTipGenerator: StandardXYToolTipGenerator = null

    if (tooltips) {
      toolTipGenerator = StandardXYToolTipGenerator.getTimeSeriesInstance()
    }

    var urlGenerator: StandardXYURLGenerator = null
    if (urls) {
      urlGenerator = new StandardXYURLGenerator()
    }

    val renderer: XYLineAndShapeRenderer = new XYLineAndShapeRenderer(true, false)
    renderer.setBaseToolTipGenerator(toolTipGenerator)
    renderer.setURLGenerator(urlGenerator)
    plot.setRenderer(renderer)

    val chart: JFreeChart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend)
    new StandardChartTheme("JFree").apply(chart)
    chart
  }
}
