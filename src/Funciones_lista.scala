import scala.annotation.tailrec

object Funciones_lista {

  // Impresión de una lista en forma de matriz a partir de esta y sus dimensiones
  @tailrec
  def imprimir_lista(width: Int, length: Int, matrix: List[Int]): Unit = {
    // Función auxiliar que imprime para una de las filas
    @tailrec
    def imprimir_lista_aux(width: Int, matrix: List[Int]): List[Int] = {
      if (width == 0) {
        matrix
      } else {
        print(matrix.head + " ")
        imprimir_lista_aux(width - 1, matrix.tail)
      }
    }
    if (length != 0) {
      val matrix_ = imprimir_lista_aux(width, matrix)
      println()
      imprimir_lista(width, length-1, matrix_)
    }
  }

  // Obtiene el valor de una posición concreta de una lista en forma de matriz a partir de la posición, la matriz y sus dimensiones
  def get(x: Int, y: Int, width: Int, length: Int, matrix: List[Int]): Int = {
    // Función auxiliar que busca el valor requerido de forma iterativa
    @tailrec
    def get_aux(n: Int, matrix: List[Int]): Int = {
      if (n == 0) {
        matrix.head
      } else {
        get_aux(n-1, matrix.tail)
      }
    }
    assert(x<length && y<width)
    get_aux(x*width+y, matrix)
  }

  // Obtiene la fila especificada a partir de la requerida, la matriz y sus dimensiones
  def row(x: Int, width: Int, length: Int, matrix: List[Int]): List[Int] = {
    // Función auxiliar que que itera por cada uno de los elementos de la lista guardando los necesarios
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
    assert(x<length)
    row_aux(0, x*width, (x+1)*width-1, matrix)
  }

  // Obtiene la columna especificada a partir de la requerida, la matriz y sus dimensiones
  def column(y: Int, width: Int, length: Int, matrix: List[Int]): List[Int] = {
    // Función auxiliar que que itera por cada uno de los elementos de la lista guardando los necesarios
    def column_aux(x: Int, y: Int, width: Int, length: Int, matrix: List[Int]): List[Int] = {
      if (x%width == y) {
        matrix.head :: column_aux(x+1, y, width, length, matrix.tail)
      } else {
        if ((width*(length-1)+y)+1 == x) {
          Nil
        } else {
          column_aux(x+1, y, width, length, matrix.tail)
        }
      }
    }
    assert(y<width)
    column_aux(0, y, width, length, matrix)
  }

  // Cuenta el número de elementos de una lista
  def len(lista: List[Int]): Int = {
    if(lista == Nil) {
      0
    } else {
      1 + len(lista.tail)
    }
  }
}
