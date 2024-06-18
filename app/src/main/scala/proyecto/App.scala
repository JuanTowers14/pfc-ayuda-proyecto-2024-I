/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package proyecto
import datos._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object App{

  def saludo() = "Proyecto final"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(vuelosCurso)

    val obj = new Itinerario()
    val vuelo1 = Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0)
    val vuelo2 = Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0)
    val vuelo3 = Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0)

    val vuelo4=Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4)

    val vuelo5=Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0)
    val vuelo6=Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0)
    val vuelo7=Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0)


    println(obj.calcularTiempoTotalDeVuelo(List(vuelo4), aeropuertosCurso))

    println(obj.calcularTiempoTotalDeVuelo(List(vuelo5,vuelo6,vuelo7), aeropuertosCurso))

    println(obj.calcularTiempoTotalDeVuelo(List(vuelo1,vuelo2,vuelo3) , aeropuertosCurso))



  }
 }
