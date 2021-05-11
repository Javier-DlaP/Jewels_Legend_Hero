import Funciones_tablero._

object IA 
{
    /*
    * AYUDA
    * Le indica al jugador que fichas recomienda mover
    */
    def ayuda(width: Int, length: Int, tablero: List[Int]): Boolean =
    {
        val matriz_recuento1 = List[Int]()   // en verdad es una lista
        val width_matriz_recuento: Int = 5  // |num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
        val matriz_recuento2 = probar_posiciones(0, 0, width, length, tablero, matriz_recuento1)

        if (matriz_recuento2.isEmpty)
        {
            println("No hay movimientos posibles")
            false
        }
        else
        {
            // coger las cabezs de las listas y mirar cual es más grande
            println("conseguido")
            true
        }
    }

    def probar_posiciones(num_fila: Int, num_columna: Int, width: Int, length: Int, tablero: List[Int], matriz_recuento: List[Int]): List[Int] =
    {
        // Se han comprobado todas las posiciones
        if (num_fila == length)
        {
            matriz_recuento.reverse
        }
        // Faltan posiciones por comprobar
        else
        {
            // Se han comprobado todos los elem de una fila
            if (num_columna == width)
            {
                probar_posiciones(num_fila + 1, 0, width, length, tablero, matriz_recuento)               // Cambiamos de fila y empezamos por la columna 0
            }
            // Faltan elems por comprobar
            else
            {
                val matriz_recuento1 = comprobar_hay_movimientos(num_fila, num_columna, width, length, tablero, matriz_recuento) // Comprobamos el intercambio de fichas adyacentes 
                probar_posiciones(num_fila, num_columna + 1, width, length, tablero, matriz_recuento1)     // Cambiamos de columna y nos mantenemos en la fila
            }
        }
    }

    /*
    * COMPROBAR HAY MOVIMIENTOS
    * Comprueba que se hay movimentos por hacer
    * Como parametro tenemos una posicion y estudiamos los cambios que puede generar al intercambiarse con sus adyacentes
    */
    def comprobar_hay_movimientos(num_fila:Int, num_columna:Int, width: Int, length: Int, tablero: List[Int], matriz_recuento: List[Int]) : List[Int] =
    {
        // Primera fila
        if (num_fila == 0)
        {
            // Primera fila y Primera columna
            if (num_columna == 0)
            {
                // Derecha
                val fila_ficha_mov1 = num_fila
                val columna_ficha_mov1 = num_columna + 1
                val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                // Abajo
                val fila_ficha_mov2 = num_fila + 1
                val columna_ficha_mov2 = num_columna
                val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                comparar_2_elems(cont1, cont2, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, matriz_recuento)
            }
            else
            {
                // Primera fila y Ultima columna
                if (num_columna == (width - 1))
                {
                    // Izquierda
                    val fila_ficha_mov1 = num_fila
                    val columna_ficha_mov1 = num_columna - 1
                    val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                    val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                    val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                    // Abajo
                    val fila_ficha_mov2 = num_fila + 1
                    val columna_ficha_mov2 = num_columna
                    val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                    val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                    val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                    comparar_2_elems(cont1, cont2, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, matriz_recuento)
                }
                // Primera fila y Cualquier otra columna
                else
                {
                    // Derecha
                    val fila_ficha_mov1 = num_fila
                    val columna_ficha_mov1 = num_columna + 1
                    val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                    val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                    val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                    // Izquierda
                    val fila_ficha_mov2 = num_fila
                    val columna_ficha_mov2 = num_columna - 1
                    val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                    val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                    val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                    // Abajo
                    val fila_ficha_mov3 = num_fila + 1
                    val columna_ficha_mov3 = num_columna
                    val tablero3_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov3, columna_ficha_mov3, width, length, tablero)
                    val tablero3_comp = comprobar_fichas_alineadas (width, length, tablero3_mov, true)
                    val cont3 = tablero3_comp.count((x:Int) => {x == -1})

                    comparar_3_elems(cont1, cont2, cont3, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, fila_ficha_mov3, columna_ficha_mov3, matriz_recuento)
                }
            }

        }
        else
        {
            // Ultima fila
            if (num_fila == (length - 1))
            {
                // Ultima fila y Primera columna
                if (num_columna == 0)
                {
                    // Derecha
                    val fila_ficha_mov1 = num_fila
                    val columna_ficha_mov1 = num_columna + 1
                    val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                    val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                    val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                    // Arriba
                    val fila_ficha_mov2 = num_fila - 1
                    val columna_ficha_mov2 = num_columna
                    val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                    val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                    val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                    comparar_2_elems(cont1, cont2, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, matriz_recuento)
                }
                else
                {
                    // Ultima fila y Ultima columna
                    if (num_columna == (width - 1))
                    {
                        // Izquierda
                        val fila_ficha_mov1 = num_fila 
                        val columna_ficha_mov1 = num_columna - 1
                        val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                        val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                        val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                        // Arriba
                        val fila_ficha_mov2 = num_fila - 1
                        val columna_ficha_mov2 = num_columna
                        val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                        val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                        val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                        comparar_2_elems(cont1, cont2, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, matriz_recuento)
                    }
                    // Ultima fila y Cualquier otra columna
                    else
                    {
                        // Derecha
                        val fila_ficha_mov1 = num_fila 
                        val columna_ficha_mov1 = num_columna + 1
                        val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                        val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                        val cont1 = tablero1_comp.count((x:Int) => {x == -1}) 

                        // Izquierda
                        val fila_ficha_mov2 = num_fila 
                        val columna_ficha_mov2 = num_columna - 1
                        val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                        val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                        val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                        // Arriba
                        val fila_ficha_mov3 = num_fila - 1
                        val columna_ficha_mov3 = num_columna
                        val tablero3_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov3, columna_ficha_mov3, width, length, tablero)
                        val tablero3_comp = comprobar_fichas_alineadas (width, length, tablero3_mov, true)
                        val cont3 = tablero3_comp.count((x:Int) => {x == -1})

                        comparar_3_elems(cont1, cont2, cont3, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, fila_ficha_mov3, columna_ficha_mov3, matriz_recuento)
                    }
                }
            }
            // Cualquier otra fila
            else
            {
                // Cualquier otra fila y Primera columna
                if (num_columna == 0)
                {
                    // Derecha
                    val fila_ficha_mov1 = num_fila 
                    val columna_ficha_mov1 = num_columna + 1
                    val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                    val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                    val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                    // Arriba
                    val fila_ficha_mov2 = num_fila - 1
                    val columna_ficha_mov2 = num_columna 
                    val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                    val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                    val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                    // Abajo
                    val fila_ficha_mov3 = num_fila + 1
                    val columna_ficha_mov3 = num_columna
                    val tablero3_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov3, columna_ficha_mov3, width, length, tablero)
                    val tablero3_comp = comprobar_fichas_alineadas (width, length, tablero3_mov, true)
                    val cont3 = tablero3_comp.count((x:Int) => {x == -1})

                    comparar_3_elems(cont1, cont2, cont3, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, fila_ficha_mov3, columna_ficha_mov3, matriz_recuento)
                }
                else
                {
                    // Cualquier otra fila y Ultima columna
                    if (num_columna == (width - 1))
                    {
                        // Izquierda
                        val fila_ficha_mov1 = num_fila 
                        val columna_ficha_mov1 = num_columna - 1
                        val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                        val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                        val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                        // Arriba
                        val fila_ficha_mov2 = num_fila - 1
                        val columna_ficha_mov2 = num_columna 
                        val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                        val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                        val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                        // Abajo
                        val fila_ficha_mov3 = num_fila + 1
                        val columna_ficha_mov3 = num_columna
                        val tablero3_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov3, columna_ficha_mov3, width, length, tablero)
                        val tablero3_comp = comprobar_fichas_alineadas (width, length, tablero3_mov, true)
                        val cont3 = tablero3_comp.count((x:Int) => {x == -1})

                        comparar_3_elems(cont1, cont2, cont3, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, fila_ficha_mov3, columna_ficha_mov3, matriz_recuento)
                    }
                    // Cualquier otra fila y Cualquier otra columna
                    else
                    {
                        // Derecha
                        val fila_ficha_mov1 = num_fila 
                        val columna_ficha_mov1 = num_columna + 1
                        val tablero1_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, width, length, tablero)
                        val tablero1_comp = comprobar_fichas_alineadas (width, length, tablero1_mov, true)
                        val cont1 = tablero1_comp.count((x:Int) => {x == -1})

                        // Izquierda
                        val fila_ficha_mov2 = num_fila 
                        val columna_ficha_mov2 = num_columna - 1
                        val tablero2_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov2, columna_ficha_mov2, width, length, tablero)
                        val tablero2_comp = comprobar_fichas_alineadas (width, length, tablero2_mov, true)
                        val cont2 = tablero2_comp.count((x:Int) => {x == -1})

                        // Arriba
                        val fila_ficha_mov3 = num_fila - 1
                        val columna_ficha_mov3 = num_columna
                        val tablero3_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov3, columna_ficha_mov3, width, length, tablero)
                        val tablero3_comp = comprobar_fichas_alineadas (width, length, tablero3_mov, true)
                        val cont3 = tablero3_comp.count((x:Int) => {x == -1})

                        // Abajo
                        val fila_ficha_mov4 = num_fila + 1
                        val columna_ficha_mov4 = num_columna
                        val tablero4_mov = mover_fichas(num_fila, num_columna, fila_ficha_mov4, columna_ficha_mov4, width, length, tablero)
                        val tablero4_comp = comprobar_fichas_alineadas (width, length, tablero4_mov, true)
                        val cont4 = tablero4_comp.count((x:Int) => {x == -1})

                        comparar_4_elems(cont1, cont2, cont3, cont4, num_fila, num_columna, fila_ficha_mov1, columna_ficha_mov1, fila_ficha_mov2, columna_ficha_mov2, fila_ficha_mov3, columna_ficha_mov3, fila_ficha_mov4, columna_ficha_mov4, matriz_recuento)
                    }
                }
            }
        }
    }


    /*
    * COMPARAR_2_ELEM
    * Mira que movimiento ha generado más -1
    * Guarda en la matriz_recuento los datos del movimiento ganador
    * |num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
    */
    def comparar_2_elems(cont1: Int, cont2: Int, num_fila: Int, num_columna: Int, fila_ficha_mov1: Int, columna_ficha_mov1: Int, fila_ficha_mov2: Int, columna_ficha_mov2: Int, matriz_recuento: List[Int]): List[Int] =
    {
        // hay movimiento
        if ((cont1 > 0) || (cont2 > 0))
        {
            if (cont1 >= cont2)
            {
                //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                val mr1 = cont1 :: matriz_recuento
                val mr2 = num_fila :: mr1
                val mr3 = num_columna :: mr2
                val mr4 = fila_ficha_mov1 :: mr3
                columna_ficha_mov1 :: mr4 
            }
            // cont1 < cont2
            else
            {
                //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                val mr1 = cont2 :: matriz_recuento
                val mr2 = num_fila :: mr1
                val mr3 = num_columna :: mr2
                val mr4 = fila_ficha_mov2 :: mr3
                columna_ficha_mov2 :: mr4 
            }
        }
        // no hay movimientos --> no se guarda nada en matriz_recuento
        else
        {
            matriz_recuento
        }
    }


    /*
    * COMPARAR_3_ELEM
    * Mira que movimiento ha generado más -1
    * Guarda en la matriz_recuento los datos del movimiento ganador
    * |num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
    */
    def comparar_3_elems(cont1: Int, cont2: Int, cont3: Int, num_fila: Int, num_columna: Int, fila_ficha_mov1: Int, columna_ficha_mov1: Int, fila_ficha_mov2: Int, columna_ficha_mov2: Int, fila_ficha_mov3: Int, columna_ficha_mov3: Int, matriz_recuento: List[Int]): List[Int] =
    {
        // hay movimiento
        if ((cont1 > 0) || (cont2 > 0) || (cont3 > 0))
        {
            // cont1
            if ((cont1 >= cont2) && (cont1 >= cont3))
            {
                //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                val mr1 = cont1 :: matriz_recuento
                val mr2 = num_fila :: mr1
                val mr3 = num_columna :: mr2
                val mr4 = fila_ficha_mov1 :: mr3
                columna_ficha_mov1 :: mr4 
            }
            // (cont1 < cont2) && (cont1 < cont3)
            else
            {
                // cont2
                if ((cont2 >= cont1) && (cont2 >= cont3))
                {
                    //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                    val mr1 = cont2 :: matriz_recuento
                    val mr2 = num_fila :: mr1
                    val mr3 = num_columna :: mr2
                    val mr4 = fila_ficha_mov2 :: mr3
                    columna_ficha_mov2 :: mr4 
                }
                // cont3
                else
                {
                    //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                    val mr1 = cont3 :: matriz_recuento
                    val mr2 = num_fila :: mr1
                    val mr3 = num_columna :: mr2
                    val mr4 = fila_ficha_mov3 :: mr3
                    columna_ficha_mov3 :: mr4 
                }
            }
        }
        // no hay movimientos --> no se guarda nada en matriz_recuento
        else
        {
            matriz_recuento
        }
    }

    /*
    * COMPARAR_4_ELEM
    * Mira que movimiento ha generado más -1
    * Guarda en la matriz_recuento los datos del movimiento ganador
    * |num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
    */
    def comparar_4_elems(cont1: Int, cont2: Int, cont3: Int, cont4: Int, num_fila: Int, num_columna: Int, fila_ficha_mov1: Int, columna_ficha_mov1: Int, fila_ficha_mov2: Int, columna_ficha_mov2: Int, fila_ficha_mov3: Int, columna_ficha_mov3: Int, fila_ficha_mov4: Int, columna_ficha_mov4: Int, matriz_recuento: List[Int]): List[Int] =
    {
        // hay movimiento
        if ((cont1 > 0) || (cont2 > 0) || (cont3 > 0) || (cont4 > 0))
        {
            // cont1
            if ((cont1 >= cont2) && (cont1 >= cont3) && (cont1 >= cont4))
            {
                //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                val mr1 = cont1 :: matriz_recuento
                val mr2 = num_fila :: mr1
                val mr3 = num_columna :: mr2
                val mr4 = fila_ficha_mov1 :: mr3
                columna_ficha_mov1 :: mr4 
            }
            else
            {
                // cont2
                if ((cont2 >= cont1) && (cont2 >= cont3) && (cont2 >= cont4))
                {
                    //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                    val mr1 = cont2 :: matriz_recuento
                    val mr2 = num_fila :: mr1
                    val mr3 = num_columna :: mr2
                    val mr4 = fila_ficha_mov2 :: mr3
                    columna_ficha_mov2 :: mr4 
                }
                // cont3
                else
                {
                    // cont3
                    if ((cont3 >= cont1) && (cont3 >= cont2) && (cont3 >= cont4))
                    {
                        //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                        val mr1 = cont3 :: matriz_recuento
                        val mr2 = num_fila :: mr1
                        val mr3 = num_columna :: mr2
                        val mr4 = fila_ficha_mov3 :: mr3
                        columna_ficha_mov3 :: mr4 
                    }
                    // cont4
                    else
                    {
                        //|num de -1|fila_ficha_ppal|columna_ficha_ppal|fila_ficha_secun|columna_ficha_secun|
                        val mr1 = cont4 :: matriz_recuento
                        val mr2 = num_fila :: mr1
                        val mr3 = num_columna :: mr2
                        val mr4 = fila_ficha_mov4 :: mr3
                        columna_ficha_mov4 :: mr4
                    }                    
                }
            }
        }
        // no hay movimientos --> no se guarda nada en matriz_recuento
        else
        {
            matriz_recuento
        }
    }
}
