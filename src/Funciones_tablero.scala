import scala.annotation.tailrec
import Funciones_lista._

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
    // Imprime las diferentes filas del tablero
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
            case 1 => print(Console.BLACK + "1")
            case 2 => print(Console.RED + "2")
            case 3 => print(Console.GREEN + "3")
            case 4 => print(Console.YELLOW + "4")
            case 5 => print(Console.BLUE + "5")
            case 6 => print(Console.MAGENTA + "6")
            case 7 => print(Console.CYAN + "7")
            case 8 => print(Console.WHITE + "8")
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

  // Hace caer los diamantes del tablero introducido y caen nuevas fichas
  def nuevos_diamantes(width: Int, length: Int, tablero: List[Int]): List[Int] = {
    // Hace caer cada diamante una posición hacia abajo hasta que todos los diamantes se encuentran sobre otro diamante
    @tailrec
    def caer_diamantes(x: Int, y: Int, width: Int, length: Int, tablero: List[Int]): List[Int] =  {
      // Comprueba si falta por caer algun diamante en todo el tablero
      @tailrec
      def quedar_por_caer(columnas_restantes: Int, width: Int, length: Int, tablero: List[Int]): Boolean = {
        // Comprueba si falta por caer algun diamante en una columna del tablero
        @tailrec
        def quedar_por_caer_columna(encima: Int, columna: List[Int]): Boolean = {
          if (columna == Nil) {
            false
          } else {
            if (columna.head != 0) {
              quedar_por_caer_columna(columna.head, columna.tail)
            } else {
              if (encima == 0) {
                quedar_por_caer_columna(columna.head, columna.tail)
              } else {
                true
              }
            }
          }
        }

        if(columnas_restantes == 0) {
          false
        } else {
          quedar_por_caer_columna(0, column(columnas_restantes-1, width, length, tablero)) ||  quedar_por_caer(columnas_restantes-1, width, length, tablero)
        }
      }

      if(length == x) {
        if(quedar_por_caer(width, width, length, tablero)) {
          caer_diamantes(0, 0, width, length, tablero)
        } else {
          tablero
        }
      } else {
        val diamante = get(x,y,width,length,tablero)
        if(x+1 != length && diamante != 0 && get(x+1,y,width,length,tablero) == 0) {
          val tablero_ = set(diamante,x+1,y,width,length,set(0,x,y,width,length,tablero))
          if(y+1 == width) {
            caer_diamantes(x+1,0,width,length,tablero_)
          } else {
            caer_diamantes(x,y+1,width,length,tablero_)
          }
        } else {
          if(y+1 == width) {
            caer_diamantes(x+1,0,width,length,tablero)
          } else {
            caer_diamantes(x,y+1,width,length,tablero)
          }
        }
      }
    }

    // Se colocan fichas con un valor entre el 1 y el 8 en las posiciones sin ninguna de estas
    def colocar_diamantes(restantes: Int, tablero: List[Int]): List[Int] = {
      if (restantes == 0) {
        tablero
      } else {
        if(tablero.head == 0) {
          val r = new scala.util.Random
          r.nextInt(8)+1::colocar_diamantes(restantes-1, tablero.tail)
        } else {
          tablero.head::colocar_diamantes(restantes, tablero.tail)
        }
      }
    }
    
    val tablero_ = caer_diamantes(0, 0, width, length, tablero)
    colocar_diamantes(count(0, tablero_), tablero_)
  }
}