package analytics

import java.awt.{Color, BorderLayout, Dimension, GraphicsEnvironment}
import java.text.{DecimalFormat, SimpleDateFormat}
import javax.swing.{JFrame, JPanel}

import common.Utils._
import org.jfree.chart.axis.{AxisLocation, DateAxis, NumberAxis}
import org.jfree.chart.labels.{XYToolTipGenerator, StandardXYToolTipGenerator}
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
  val ohlcData: ListBuffer[(String, Seq[DataItem])] = ListBuffer()
  val subData: ListBuffer[(String, Seq[DataItem])] = ListBuffer()
  val frame: JFrame = new JFrame("Plot")

  def createChart(chartTitle: String): JFreeChart = {

    val dateAxis: DateAxis = new DateAxis("Date")
    dateAxis.setDateFormatOverride(new SimpleDateFormat("dd.MM"))

    dateAxis.setLowerMargin(0.02)
    dateAxis.setUpperMargin(0.02)


    val chart: JFreeChart = createTimeSeriesChart("", "Date", "OHLC", null, true)

    val plot: XYPlot = new XYPlot(null, dateAxis, new NumberAxis("OHLC"), new SamplingXYLineRenderer())

    ohlcData.foreach(aa => {
      val plotCount = plot.getRendererCount
      val candlestickDataset: OHLCSeriesCollection = new OHLCSeriesCollection()
      val ohlcSeries = new OHLCSeries(aa._1)

      aa._2.foreach(bb => {
        val date = new FixedMillisecond(str2date(bb("Date")).toDate)
        ohlcSeries.add(date, bb.toDouble("Open"), bb.toDouble("High price"), bb.toDouble("Low price"), bb.toDouble("Closing price"));
      })

      candlestickDataset.addSeries(ohlcSeries)

      plot.setDataset(plotCount + 1, candlestickDataset)
      plot.setRenderer(plotCount + 1, new CandlestickRenderer(CandlestickRenderer.WIDTHMETHOD_AVERAGE, false,
        new StandardXYToolTipGenerator("{2} - {1}", new SimpleDateFormat("dd.MM.yyy"), new DecimalFormat("0.00"))))
    })

    data.foreach(dat => {
      val volumeDataset: TimeSeriesCollection = new TimeSeriesCollection()
      val plotCount = plot.getRendererCount
      val volumeSeries: TimeSeries = new TimeSeries(dat._2(0).source + ":" + dat._1 + ":" + plotCount)
      val volumeAxis: NumberAxis = new NumberAxis(dat._2(0).source + ":" + dat._1 + ":" + plotCount)
      volumeAxis.setNumberFormatOverride(new DecimalFormat("0"))

      dat._2.foreach(dataItem => {
        val date = new FixedMillisecond(str2date(dataItem("Date")).toDate)
        volumeSeries.add(date, dataItem.toDouble(dat._1));
      })

      println("Added size:" + dat._2.size + " tag:" + dat._1 + " total:" + data.size)
      volumeDataset.addSeries(volumeSeries)
      plot.setDataset(plotCount + 1, volumeDataset)
      val generator = new StandardXYItemRenderer()
      generator.setBaseToolTipGenerator(new XYToolTipGenerator() {
        override def generateToolTip(xyDataset: XYDataset, series: Int, category: Int): String = {
          //          println(xyDataset.getXValue(series, category) + ":" + xyDataset.getYValue(series, category))
          xyDataset.getYValue(series, category).toString
        }
      })

      plot.setRenderer(plotCount + 1, generator)
    })

    plot.setDomainGridlinesVisible(true)
    plot.setDomainMinorGridlinesVisible(true)
    plot.setRangeAxisLocation(AxisLocation.BOTTOM_OR_RIGHT)
    //    ChartFactory.getChartTheme().apply(chart)
    chart.getXYPlot.asInstanceOf[CombinedDomainXYPlot].add(plot, 3)


    if (!subData.isEmpty) {

      val subPlot: XYPlot = new XYPlot(null, dateAxis, new NumberAxis("Series"), new SamplingXYLineRenderer())
      subData.foreach(dat => {
        val volumeDataset: TimeSeriesCollection = new TimeSeriesCollection()

        val volumeSeries: TimeSeries = new TimeSeries(dat._2(0).source + ":" + dat._1 + ":" + subPlot.getRendererCount)
        val volumeAxis: NumberAxis = new NumberAxis(dat._2(0).source + ":" + dat._1 + ":" + subPlot.getRendererCount)
        volumeAxis.setNumberFormatOverride(new DecimalFormat("0"))

        dat._2.foreach(dataItem => {
          val date = new FixedMillisecond(str2date(dataItem("Date")).toDate)
          volumeSeries.add(date, dataItem.toDouble(dat._1));
        })

        println("Added size:" + dat._2.size + " tag:" + dat._1 + " total:" + data.size)
        volumeDataset.addSeries(volumeSeries)
        subPlot.setDataset(subPlot.getRendererCount + 1, volumeDataset)
        subPlot.setRenderer(subPlot.getRendererCount + 1, new StandardXYItemRenderer())
      })
      subPlot.setDomainGridlinesVisible(true)
      subPlot.setDomainMinorGridlinesVisible(true)
      subPlot.setRangeAxisLocation(AxisLocation.BOTTOM_OR_RIGHT)
      chart.getXYPlot.asInstanceOf[CombinedDomainXYPlot].add(subPlot, 1)
    }

    //    val subplots = chart.getXYPlot.asInstanceOf[CombinedDomainXYPlot].getSubplots.iterator()
    //    while (subplots.hasNext) {
    //      val ssublot = subplots.next().asInstanceOf[XYPlot]
    //      ssublot.setBackgroundPaint(Color.white)
    //      ssublot.setDomainGridlinePaint(Color.black)
    //      ssublot.setRangeGridlinePaint(Color.black);
    //      ssublot.setDomainGridlinesVisible(true)
    //      ssublot.setDomainMinorGridlinesVisible(true)
    //    }

    //    chart.getXYPlot.setBackgroundPaint(Color.lightGray)
    //    chart.getXYPlot.setDomainGridlinePaint(Color.black);
    //    chart.getXYPlot.setRangeGridlinePaint(Color.black);
    //    chart.getXYPlot.setDomainGridlinesVisible(true)
    //    chart.getXYPlot.setDomainMinorGridlinesVisible(true)


    //TODO: http://www.jfree.org/forum/viewtopic.php?f=3&t=115213
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

  def addCandleSeries(items: Seq[DataItem], dataName: String = "Volume"): PlotBatchV2 = {
    ohlcData += ((dataName, items))
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
                            legend: Boolean): JFreeChart = {
    val timeAxis: DateAxis = new DateAxis(timeAxisLabel)
    timeAxis.setDateFormatOverride(new SimpleDateFormat("dd.MM.yyyy"))
    timeAxis.setLowerMargin(0.02D)
    timeAxis.setUpperMargin(0.02D)

    val valueAxis: NumberAxis = new NumberAxis(valueAxisLabel)
    valueAxis.setAutoRangeIncludesZero(false)

    val plot: CombinedDomainXYPlot = new CombinedDomainXYPlot(timeAxis)
    plot.setOrientation(PlotOrientation.VERTICAL);

    plot.setGap(3.0)

    val renderer: XYLineAndShapeRenderer = new XYLineAndShapeRenderer(true, false)
    renderer.setBaseToolTipGenerator(null) //StandardXYToolTipGenerator.getTimeSeriesInstance()
    renderer.setURLGenerator(null) //new StandardXYURLGenerator()
    plot.setRenderer(renderer)


    val chart: JFreeChart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend)
    //    ChartFactory.getChartTheme().apply(chart)
    StandardChartTheme.createLegacyTheme().apply(chart)
    chart
  }
}
