import scala.io.Source

import breeze.linalg._
import breeze.plot._

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

  val zStar: DenseVector[Double] = new DenseVector(dataX.map { x =>
    xStar(0) * x * x + xStar(1) * x + xStar(2);
  });

  val f = Figure();
  val p = f.subplot(0);
  p += plot(dataX, dataZ, '.');
  p += plot(dataX, zStar);
  p.title = "Quadratic Approximation of a Tennis Ball Flight";
  p.xlabel = "X (feet)";
  p.ylabel = "Z (feet)";
  f.saveas("plot.png");

}
