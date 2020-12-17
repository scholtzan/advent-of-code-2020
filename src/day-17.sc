import scala.annotation.tailrec

val input = "#.#.##.#\n#.####.#\n...##...\n#####.##\n#....###\n##..##..\n#..####.\n#...#.#."

val n = 6

val emptyZLayers = Array.fill(n)(Array.fill(input.split("\n").length + n * 2)(Array.fill(input.split("\n").head.length + n * 2)(false)))
val inbetween = Array.fill(input.split("\n").length)(Array.fill(n * 2 + 8)(false)) ++ input.split("\n").map { row =>
  Array.fill(n)(false) ++ row.split("").map(_ == "#") ++ Array.fill(n)(false)
} ++ Array.fill(input.split("\n").length)(Array.fill(n * 2 + 8)(false))


val cubes = emptyZLayers ++ Array(inbetween) ++ emptyZLayers

val deltaCombos = (-1 to 1).flatMap(a => (-1 to 1).flatMap(b => (-1 to 1).map(c => Vector(a,b,c)))).filter(_ != Vector(0,0,0)).distinct

// part 1

@tailrec
def cycle(state: Array[Array[Array[Boolean]]], n: Int): Int = {
  if (n <= 0) {
    return state.flatten.flatten.count(_ == true)
  }

  val afterCycle = state.zipWithIndex.map { case (z, zi) =>
    z.zipWithIndex.map { case (y, yi) =>
      y.zipWithIndex.map { case (x, xi) =>
        val activeNeighbours = deltaCombos.count { d =>
          if (zi + d(0) >= 0 && yi + d(1) >= 0 && xi + d(2) >= 0 &&
            zi + d(0) < state.length && yi + d(1) < state.head.length && xi + d(2) < state.head.head.length) {
            state(zi + d(0))(yi + d(1))(xi + d(2))
          } else {
            false
          }
        }

        if (x) {
          if (activeNeighbours == 2 || activeNeighbours == 3) {
            true
          } else {
            false
          }
        } else {
          if (activeNeighbours == 3) {
            true
          } else {
            false
          }
        }
      }
    }
  }

  cycle(afterCycle, n - 1)
}

val result = cycle(cubes, 6)

// part 2


val emptyHyperCubes = Array.fill(n)(Array.fill(cubes.length)(Array.fill(cubes.head.length)(Array.fill(cubes.head.head.length)(false))))
val cubes2 = emptyHyperCubes ++ Array(emptyZLayers ++ Array(inbetween) ++ emptyZLayers) ++ emptyHyperCubes

val deltaCombos2 = (-1 to 1).flatMap(a => (-1 to 1).flatMap(b => (-1 to 1).flatMap(c => (-1 to 1).map(d => Vector(a,b,c,d))))).filter(_ != Vector(0,0,0,0)).distinct

// part 1

@tailrec
def cycle2(state: Array[Array[Array[Array[Boolean]]]], n: Int): Int = {
  if (n <= 0) {
    return state.flatten.flatten.flatten.count(_ == true)
  }

  val afterCycle = state.zipWithIndex.map { case (w, wi) =>
    w.zipWithIndex.map { case (z, zi) =>
      z.zipWithIndex.map { case (y, yi) =>
        y.zipWithIndex.map { case (x, xi) =>
          val activeNeighbours = deltaCombos2.count { d =>
            if (wi + d(0) >= 0 && zi + d(1) >= 0 && yi + d(2) >= 0 && xi + d(3) >= 0 &&
              wi + d(0) < state.length && zi + d(1) < state.head.length && yi + d(2) < state.head.head.length && xi + d(3) < state.head.head.head.length) {
              state(wi + d(0))(zi + d(1))(yi + d(2))(xi + d(3))
            } else {
              false
            }
          }

          if (x) {
            if (activeNeighbours == 2 || activeNeighbours == 3) {
              true
            } else {
              false
            }
          } else {
            if (activeNeighbours == 3) {
              true
            } else {
              false
            }
          }
        }
      }
    }
  }

  cycle2(afterCycle, n - 1)
}

val result2 = cycle2(cubes2, 6)