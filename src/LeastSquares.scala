import scala.io.Source

import breeze.linalg._
import breeze.plot._
import org.jfree.chart.plot.ValueMarker

object LeastSquares extends App {

  val filename = "data.txt";

  val data: Array[Array[Double]] = Source.fromFile(filename).getLines.toArray.map { line =>
    val Array(step, x, y) = line.split("\\s+");
    Array(x.toDouble, y.toDouble)
  }

  val dataX: Array[Double] = data.map(_(0));
  val dataZ: Array[Double] = data.map(_(1));

  // DenseMatrix constructor is column major, construct 3 row x 101 column, then transpose
  val A: DenseMatrix[Double] = new DenseMatrix(3, 101, dataX.map { x =>
    // c1 * x^2 + c2 * x + c3
    Array(x * x, x, 1);
  }.flatten).t;

  val B: DenseMatrix[Double] = new DenseMatrix(101, 1, dataZ);

  val xStar: DenseVector[Double] = (inv(A.t * A) * A.t * B).toDenseVector;

  def zStar(domain: Array[Double]): Array[Double] = domain.map { x =>
    xStar(0) * x * x + xStar(1) * x + xStar(2);
  };

  // Error e of estimation using n samples
  def e(z: Array[Double], zs: Array[Double], n: Int): Double = {
    val sum: Double = (z.take(n), zs.take(n)).zipped.map(_-_).map(x => x * x).sum;
    Math.sqrt(sum) / n;
  }

  val N: Array[Int] = 1.to(data.size).toArray

  val error: Array[Double] = N.map { n => e(dataZ, zStar(dataX), n) };

  val targetError: Double = 3.0e-3;
  val minSamples: Int = error.zipWithIndex.find { case (e, i) => e < targetError }.get._2;
  println("Minimum samples: " + minSamples + ", error: " + error(minSamples));


  // Plots

  // Estimation
  val fitFigure = Figure();
  val domain: Array[Double] = (0 to 20).toArray.map(_.toDouble)
  fitFigure.width = 1200;
  fitFigure.height = 800;
  val fitPlot = fitFigure.subplot(0);
  fitPlot += plot(dataX, dataZ, '.');
  fitPlot += plot(domain, zStar(domain));
  fitPlot += plot(domain, domain.map { n => 0.0 }, '-');
  fitPlot.title = "Quadratic Approximation of a Tennis Ball Flight";
  fitPlot.xlabel = "X (feet)";
  fitPlot.ylabel = "Z (feet)";
  fitFigure.saveas("fit.png", 96);

  // Error
  val errorFigure = Figure();
  errorFigure.width = 1200;
  errorFigure.height = 800;
  val errorPlot = errorFigure.subplot(0);
  errorPlot += plot(N.map { n => n.toDouble }, error, '-');
  errorPlot += plot(N.map { n => n.toDouble }, N.map { n => 3.0e-3 }, '-');
  errorPlot.plot.addDomainMarker(new ValueMarker(minSamples));
  errorPlot.title = "Error as a function of N";
  errorPlot.xlabel = "N (samples)";
  errorPlot.ylabel = "Error (feet)";
  errorFigure.saveas("error.png", 96);

}
