class Funciones_lista {
  
    def imprimir(length: Int, matrix: List[Int]) {
      def imprimir_aux(width: Int, matrix: List[Int]): List[Int] = {
        if (width == 0) {
          matrix
        } else {
          print(matrix.head + " ")
          imprimir_aux(width - 1, matrix.tail)
        }
      }

      if (matrix != Nil) {
        val matrix_ = imprimir_aux(length, matrix)
        println()
        imprimir(length, matrix_)
      }
    }

  def get(x: Int, y: Int, length: Int, matrix: List[Int]): Int = {
    def get_aux(n: Int, matrix: List[Int]): Int = {
      if (n == 0) {
        matrix.head
      } else {
        get_aux(n-1, matrix.tail)
      }
    }
    get_aux(x*length+y, matrix)
  }

  def row(x: Int, length: Int, matrix: List[Int]): List[Int] = {
    def row_aux(x: Int, a: Int, b: Int, matrix: List[Int]): List[Int] = {
      if (x >= a && x <= b) {
        matrix.head :: row_aux(x+1, a, b, matrix.tail)
      } else {
        if (x < a) {
          row_aux(x+1, a, b, matrix.tail)
        } else {
          Nil
        }
      }
    }
    row_aux(0, x*length, (x+1)*length-1, matrix)
  }

  def column(y: Int, length: Int, matrix: List[Int]): List[Int] = {
    def column_aux(x: Int, y: Int, matrix: List[Int]): List[Int] = {
      if (x%length == y) {
        matrix.head :: column_aux(x+1, y, matrix.tail)
      } else {
        if ((length*(length-1)+y)+1 == x) {
          Nil
        } else {
          column_aux(x+1, y, matrix.tail)
        }
      }
    }
    column_aux(0, y, matrix)
  }
}
