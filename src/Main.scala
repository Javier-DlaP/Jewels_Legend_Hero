import Funciones_tablero._
import Funciones_lista._

object Main {
  def main(args: Array[String]): Unit = {
    val width = 7
    val length = 9

    val fila_inicial = 1
    val columna_inicial = 1
    val fila_final = 2
    val columna_final = 1

    val tablero = crear_tablero(width, length)
    //imprimir_tablero(width, length, tablero)

    val tablero_lleno = nuevos_diamantes(width, length, tablero)
    //imprimir_tablero(width, length, tablero_lleno)

    val tablero_ = set(0,4,3,width,length,set(0,3,4,width,length,set(0,3,3,width,length,tablero_lleno)))
    //imprimir_tablero(width, length, tablero_)

    val tablero_lleno_ = nuevos_diamantes(width, length, tablero_)
    println("Nuevos diamantes")
    imprimir_tablero(width, length, tablero_lleno_)

    val tablero1 = mover_fichas(fila_inicial, columna_inicial, fila_final, columna_final, width, length, tablero_lleno_)
    println("Mover fichas")
    imprimir_tablero(width, length, tablero1)

    val tablero2 = comprobar_fichas_alineadas (width, length, tablero1)
    println("Comprobar fichas alineadas --> eliminaciones")
    //imprimir_tablero(width, length, tablero2)

    val tablero3 = comprobar_hay_cambios (fila_inicial, columna_inicial, fila_final, columna_final, width, length, tablero1, tablero2)
    //println("Hay retroceso?")
    imprimir_tablero(width, length, tablero3)
  }

}