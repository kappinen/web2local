package analytics

import java._
import java.awt._
import java.text.{DateFormat, DecimalFormat, SimpleDateFormat}
import javax.swing.{JFrame, JPanel}

import common.Utils._
import org.jfree.chart.axis.{DateAxis, NumberAxis}
import org.jfree.chart.labels.StandardXYToolTipGenerator
import org.jfree.chart.plot.{CombinedDomainXYPlot, PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.{CandlestickRenderer, XYBarRenderer}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.time.{FixedMillisecond, TimeSeries, TimeSeriesCollection}
import org.jfree.data.time.ohlc.{OHLCSeries, OHLCSeriesCollection}
import sources.stock.GoogleHistorical
import types.DataItem

/**
 * Created by home on 27.8.2016.
 */
class PlotInternal(title: String = "") extends JPanel {
  val ohlcSeries: OHLCSeries = new OHLCSeries("Price")
  val volumeSeries: TimeSeries = new TimeSeries("Volume")

  {
    val chartPanel: ChartPanel = new ChartPanel(createChart(title));
    //    val size = Toolkit.getDefaultToolkit().getScreenSize()

    val devices = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices
    chartPanel.setPreferredSize(new Dimension(devices(0).getDisplayMode.getWidth, devices(0).getDisplayMode.getHeight - 70));

    chartPanel.setMouseZoomable(true)
    chartPanel.setMouseWheelEnabled(true)
    add(chartPanel, BorderLayout.CENTER)
  }

  //https://examples.javacodegeeks.com/desktop-java/jfreechart/jfree-candlestick-chart-example/
  def createChart(chartTitle: String): JFreeChart = {

    val candlestickSubplot: XYPlot = {
      val candlestickDataset: OHLCSeriesCollection = new OHLCSeriesCollection()
      candlestickDataset.addSeries(ohlcSeries)
      //    new CustomHighLowItemLabelGenerator(new SimpleDateFormat("kk:mm"), new DecimalFormat("0.000"))

      new XYPlot(candlestickDataset,
        null,
        new NumberAxis("Price"),
        new CandlestickRenderer(CandlestickRenderer.WIDTHMETHOD_AVERAGE, false,
          new StandardXYToolTipGenerator("{1},{2}", new SimpleDateFormat("dd.MM"), new DecimalFormat("0.00"))))
    }
    candlestickSubplot.getRenderer.setBaseToolTipGenerator(new StandardXYToolTipGenerator("{1},{2}", new SimpleDateFormat("dd.MM"), new DecimalFormat("0.00")))


    val volumeSubplot: XYPlot = {
      val volumeDataset: TimeSeriesCollection = new TimeSeriesCollection()
      volumeDataset.addSeries(volumeSeries)

      val volumeAxis: NumberAxis = new NumberAxis("Volume")
      //      volumeAxis.setAutoRangeIncludesZero(false)
      volumeAxis.setNumberFormatOverride(new DecimalFormat("0"))

      val timeRenderer: XYBarRenderer = new XYBarRenderer() //TODO: add SamplingXYLineRenderer
      timeRenderer.setShadowVisible(false)
      timeRenderer.setBaseToolTipGenerator(new StandardXYToolTipGenerator("Volume--> Time={1} Size={2}", new SimpleDateFormat("kk:mm"), new DecimalFormat("0")))

      new XYPlot(volumeDataset, null, volumeAxis, timeRenderer)
    }


    val dateAxis: DateAxis = new DateAxis("Date")
    dateAxis.setDateFormatOverride(new SimpleDateFormat("dd.MM"))

    dateAxis.setLowerMargin(0.02)
    dateAxis.setUpperMargin(0.02)

    val mainPlot: CombinedDomainXYPlot = new CombinedDomainXYPlot(dateAxis)
    mainPlot.setGap(3.0)
    mainPlot.add(candlestickSubplot, 3);
    mainPlot.add(volumeSubplot, 1);
    mainPlot.setOrientation(PlotOrientation.VERTICAL);


    val chart: JFreeChart = new JFreeChart(chartTitle, JFreeChart.DEFAULT_TITLE_FONT, mainPlot, true)
    //    chart.removeLegend()
    chart
  }


  def addCandel(dataItem: DataItem) {
    try {
      // Add bar to the data. Let's repeat the same bar
      val date = new FixedMillisecond(str2date(dataItem("Date")).toDate)
      ohlcSeries.add(date, dataItem.toDouble("Open"), dataItem.toDouble("High price"), dataItem.toDouble("Low price"), dataItem.toDouble("Closing price"));
      volumeSeries.add(date, dataItem.toDouble("Volume"));
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def addOHLCSeries(items: Seq[DataItem]): PlotInternal = {
    items.foreach(dataItem => {
        ohlcSeries.add(new FixedMillisecond(str2date(dataItem("Date")).toDate),
          dataItem.toDouble("Open"),
          dataItem.toDouble("High price"),
          dataItem.toDouble("Low price"),
          dataItem.toDouble("Closing price"))
    })
    this
  }

  def addBarSeries(items: Seq[DataItem], dataName:String = "Volume"): PlotInternal = {
    items.foreach(dataItem => {
      volumeSeries.add(new FixedMillisecond(str2date(dataItem("Date")).toDate), dataItem.toDouble(dataName))
    })
    this
  }


  def plot() = {
    //    JFrame.setDefaultLookAndFeelDecorated(false)
    val frame: JFrame = new JFrame("Plot")
//    val plot = new PlotInternal()

    frame.setContentPane(this)
    frame.pack();
    frame.setVisible(true);

  }




}

