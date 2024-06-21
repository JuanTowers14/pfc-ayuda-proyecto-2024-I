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

    val prueba = new BenchMark()

    prueba.itinerariosSalidaBenchmark()

  }
 }
