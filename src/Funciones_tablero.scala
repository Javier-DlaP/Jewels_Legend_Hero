import scala.annotation.tailrec

object Funciones_tablero {

  // Creación de un tablero de las dimensiones introducidas
  def crear_tablero(width: Int, length: Int): List[Int] = {
    // Función recursiva auxiliar que crea un tablero
    def crear_tablero_aux(n: Int): List[Int] = {
      if(n == 0) {
        Nil
      } else {
        0::crear_tablero_aux(n-1)
      }
    }
    crear_tablero_aux(width*length)
  }

  // Imprime con diferentes colores el tablero introducido en forma de lista
  def imprimir_tablero(width: Int, length: Int, tablero: List[Int]): Unit = {
    // Imprime una línea horizontal con el número de puntos introducido
    @tailrec
    def puntos(n: Int): Unit = {
      if(n == 0) {
        println()
      } else {
        print("·")
        puntos(n-1)
      }
    }
    // Imrprime las diferentes filas del tablero
    @tailrec
    def imprimir_filas_tablero(width: Int, length: Int, tablero: List[Int]): Unit = {
      // Imprime cada un de las filas del tablero
      @tailrec
      def imprimir_fila_tablero(width: Int, tablero: List[Int]): List[Int] = {
        if (width == 0) {
          print(Console.RESET + "·")
          tablero
        } else {
          print(Console.RESET + "·")
          (tablero.head: Int) match {
            case 0 => print(" ")
            case 1 => print(Console.BLACK + "0")
            case 2 => print(Console.RED + "0")
            case 3 => print(Console.GREEN + "0")
            case 4 => print(Console.YELLOW + "0")
            case 5 => print(Console.BLUE + "0")
            case 6 => print(Console.MAGENTA + "0")
            case 7 => print(Console.CYAN + "0")
            case 8 => print(Console.WHITE + "0")
            case -1 => println("Imprimiendo un valor no definido\n")
            case _ => println("Error\n")
          }
          imprimir_fila_tablero(width - 1, tablero.tail)
        }
      }

      if (length != 0) {
        val tablero_ = imprimir_fila_tablero(width, tablero)
        println()
        imprimir_filas_tablero(width, length - 1, tablero_)
      }
    }
    puntos(width*2+1)
    imprimir_filas_tablero(width, length, tablero)
    puntos(width*2+1)
  }
}
