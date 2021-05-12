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
        val profundidad_busqueda = 3

        val movimientos = movimientos_posibles(profundidad_busqueda, width, length, tablero)
        val movimientos_puntuados = movimientos.map((x) => (x._1, x._2, x._3, x._4, x._5, x._5.count(_ == -1)))
        val movimientos_filtrados = movimientos_puntuados.filter(_._6 > 0)
        
        // Se utiliza fold y no reduce para que ocurra ningún error en el caso de que solo haya un movimiento
        // El valor inicial elegido es uno que siempre será peor que el resto (-1)
        val recomendacion = movimientos_filtrados.fold((-1,-1,-1,-1,List(),-1))((x, y) => if(x._6>y._6){x}else{y})
        (recomendacion._1, recomendacion._2, recomendacion._3, recomendacion._4)   
    }

    /*
    * MOVIMIENTOS_POSIBLES
    * Genera un ParVector de tuplas que contienen el movimiento actual de la ficha junto con su tablero asociado
    * El formato de las tuplas es: (x0, y0, x1, y1, tablero)
    */
    def movimientos_posibles(profundidad:Int, width: Int, length: Int, tablero: List[Int]): ParVector[(Int, Int, Int, Int, List[Int])] =
    {

        /*
        * MOVIMIENTOS_POSIBLES_AUX
        * Itera por cada posición del tablero para generar un nodo para cada tablero al que se puede llegar
        */
        def movimientos_posibles_aux(profundidad:Int, x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
        {

            /*
            * MOVIMIENTO_DERECHA
            * Genera un nodo si el posible mover el dimante a la derecha
            */
            def movimiento_derecha(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
            {
                if(y+1 != width)
                {
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x, y+1, width, length, tablero))
                    if(nuevo_tablero == tablero)
                    {
                        nodos :+ (x, y, x, y+1, nuevo_tablero)
                    } 
                    else 
                    {
                        nodos
                    }
                } 
                else 
                {
                    nodos
                }
            }

            /*
            * MOVIMIENTO_ABAJO
            * Genera un nodo si el posible mover el dimante abajo
            */
            def movimiento_abajo(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int])]): ParVector[(Int, Int, Int, Int, List[Int])] =
            {
                if(x+1 != length)
                {
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x+1, y, width, length, tablero))
                    nodos :+ (x, y, x+1, y, nuevo_tablero)
                } 
                else 
                {
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
                if(tablero == nuevo_tablero) 
                {
                    tablero
                } 
                else 
                {
                    eliminar_fichas_ia(width, length, nuevo_tablero)
                }
            }
            
            val vector1 = movimiento_derecha(x, y, width, length, tablero, nodos)
            val vector2 = movimiento_abajo(x, y, width, length, tablero, nodos)

            // no hay mas que comprobar
            if(x+1 == length && y+1 == width)
            {
                vector2
            } 
            else 
            {
                if (profundidad == 0)
                {
                    movimientos_posibles_aux(3, if(y+1==width){x+1}else x, if(y+1==width){0}else y+1, width, length, tablero, vector2)
                }
                else
                {
                    movimientos_posibles_aux(profundidad - 1, x, y, width, length, tablero, vector2)
                }
            }
        }

        movimientos_posibles_aux(profundidad, 0,0,width,length,tablero, new ParVector())
    }
}
