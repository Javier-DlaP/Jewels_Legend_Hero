import Funciones_tablero._
import scala.collection.parallel.immutable.ParVector

import Funciones_lista._

object IA 
{

    /*
    * AYUDA
    * Le indica al jugador que fichas intercambiar
    * El formato de la tupla de salida es: (x0, y0, x1, y1)
    */
    def ayuda(width: Int, length: Int, tablero: List[Int]): (Int, Int, Int, Int) =
    {
        val movimientos = movimientos_posibles(width, length, tablero)
        val recomendacion = movimientos.filter(_._5.count(_ == -1) > 0).reduce((x, y) => if(x._5.count(_ == -1)>y._5.count(_ == -1)){x}else{y})
        if(movimientos.isEmpty){
            (-1, -1, -1, -1)
        } else {
            (recomendacion._1, recomendacion._2, recomendacion._3, recomendacion._4)
        }
    }

    /*
    * MOVIMIENTOS_POSIBLES
    * Genera un ParVector de tuplas que contienen el movimiento actual de la ficha junto con su tablero asociado
    * El formato de las tuplas es: (x0, y0, x1, y1, tablero)
    */
    def movimientos_posibles(width: Int, length: Int, tablero: List[Int]): ParVector[(Int, Int, Int, Int, List[Int])] =
    {

        /*
        * MOVIMIENTOS_POSIBLES_AUX
        * Itera por cada posición del tablero para generar un nodo para cada tablero al que se puede llegar
        */
        def movimientos_posibles_aux(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
        {

            /*
            * MOVIMIENTO_DERECHA
            * Genera un nodo si el posible mover el dimante a la derecha
            */
            def movimiento_derecha(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
            {
                if(y+1 != width){
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x, y+1, width, length, tablero))
                    if(nuevo_tablero == tablero){
                        nodos :+ (x, y, x, y+1, nuevo_tablero)
                    } else {
                        nodos
                    }
                } else {
                    nodos
                }
            }

            /*
            * MOVIMIENTO_ABAJO
            * Genera un nodo si el posible mover el dimante abajo
            */
            def movimiento_abajo(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
            {
                if(x+1 != length){
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x+1, y, width, length, tablero))
                    nodos :+ (x, y, x+1, y, nuevo_tablero)
                } else {
                    nodos
                }
            }
            
            /*
            * ELIMINAR_FICHAS_IA
            * Borra las fichas alineadas y llena las posiciones de diamantes -1 comprobando que tras caer los diamantes no se puedan eliminar aún más
            */
            def eliminar_fichas_ia(width: Int, length: Int, tablero: List[Int]): List[Int] =
            {
                val nuevo_tablero = comprobar_fichas_alineadas (width, length, tablero, true)
                if(tablero == nuevo_tablero) {
                    tablero
                } else {
                    eliminar_fichas_ia(width, length, nuevo_tablero)
                }
            }
            
            val vector1 = movimiento_derecha(x, y, width, length, tablero, nodos)
            val vector2 = movimiento_abajo(x, y, width, length, tablero, nodos)

            if(x+1 == length && y+1 == width){
                vector2
            } else {
                movimientos_posibles_aux(if(y+1==width){x+1}else x, if(y+1==width){0}else y+1, width, length, tablero, vector2)
            }
        }
        movimientos_posibles_aux(0,0,width,length,tablero, new ParVector())
    }
}
