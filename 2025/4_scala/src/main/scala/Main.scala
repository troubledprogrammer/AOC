import scala.io.Source

val SAMPLE_INPUT = """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."""

def load_input(test: Boolean): String = {
  if test then SAMPLE_INPUT else Source.fromFile("../../data/2025/4").mkString
}

def parse_input(input: String): Array[String] = {
  input.split("\n").map(_.trim())
}

def num_neighbours(row: Int, col: Int, data: Array[String]): Int = {
  val rows = data.length
  val cols = data(0).length

  List((0, 1), (1, 0), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)).count((dr, dc) => {
    val r = row + dr    
    val c = col + dc

    0 <= r && r < rows && 0 <= c  && c < cols && data(r)(c) == '@'
  })
}

// returns number of rolls that were removed
// will overcount part a as uses most up to data data even half way through a pass
def count_rolls(data: Array[String], remove: Boolean): Int = {
  data.zipWithIndex.flatMap((row, ri) => {
    row.zipWithIndex.collect {
      case ('@', ci) if num_neighbours(ri, ci, data) < 4 => {
        if (remove) data(ri) = data(ri).patch(ci, ".", 1)
        1
      }
    }

  }).sum
}

def run(data: Array[String]): (Int, Int) = {
  val a = count_rolls(data, false)

  var b = 0
  var diff = 1
  while (diff != 0) do {
    diff = count_rolls(data, true)
    b += diff
  } 

  return (a, b)
}

@main def main(): Unit =
  val input = parse_input(load_input(false))
  val (a, b) = run(input)

  printf(s"Part a $a\nPart b $b\n")