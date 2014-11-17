object LeastSquares extends App {

  val filename = "data.txt";

  for (line <- scala.io.Source.fromFile(filename).getLines()) {
    val Array(step, x, y) = line.split("\\s+");
    println(s"$step, $x, $y");
  }

}
