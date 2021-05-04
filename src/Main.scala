import Funciones_tablero._
import Funciones_lista._

object Main {
  def main(args: Array[String]): Unit = {
    val width = 7
    val length = 9

    val tablero = crear_tablero(width, length)
    imprimir_tablero(width, length, tablero)

    val tablero_lleno = nuevos_diamantes(width, length, tablero)
    imprimir_tablero(width, length, tablero_lleno)

    val tablero_ = set(0,4,3,width,length,set(0,3,4,width,length,set(0,3,3,width,length,tablero_lleno)))
    imprimir_tablero(width, length, tablero_)

    val tablero_lleno_ = nuevos_diamantes(width, length, tablero_)
    imprimir_tablero(width, length, tablero_lleno_)
  }
}