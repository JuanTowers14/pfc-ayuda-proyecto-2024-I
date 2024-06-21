/**
 * Plantilla para pruebas
 * @author Carlos Delgado
 * @version 1.0
 */

package proyecto
import datos._

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestPruebasPar2 extends AnyFunSuite {

  val itoObj = new ItinerarioPar()
  val itsCurso = itoObj.itinerariosTiempoPar(vuelosCurso, aeropuertosCurso)
  val itsCurso15 = itoObj.itinerariosTiempoPar(vuelosA1, aeropuertos)
  val itsCurso40 = itoObj.itinerariosTiempoPar(vuelosB1, aeropuertos)
  val itsCurso100 = itoObj.itinerariosTiempoPar(vuelosC1, aeropuertos)

  test("test1") {
    val its1 = itsCurso("MID", "SVCS")
    assert(its1 === List(List(Vuelo("AIRVZLA", 601, "MID", 5, 0, "SVCS", 6, 0, 0))))
  }

  test("test2") {
    val its2 = itsCurso("CLO", "SVCS")
    assert(its2 === List())
  }

  test("test3") {
    val its3 = itsCurso("CLO", "SVO")
    assert(its3 === List(
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
        Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
        Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0)),
    )
    )
  }

  test("test4") {
    val its4 = itsCurso("CLO", "MEX")
    assert(its4 === List(
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("VIVA", 756, "BOG", 9, 0, "MDE", 10, 0, 0),
        Vuelo("VIVA", 769, "MDE", 11, 0, "BAQ", 12, 0, 0),
        Vuelo("AVA", 5643, "BAQ", 14, 0, "MEX", 16, 0, 0)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("LATAM", 787, "BOG", 17, 0, "MEX", 19, 0, 0))
    )
    )
  }

  test("test5") {
    val its5 = itsCurso("CTG", "PTY")
    assert(its5.take(2) === List(
      List(Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0)),
      List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0))))
  }

  test("test6") {
    val its6 = itsCurso15("HOU", "BNA")
    assert(its6.take(2) === List(List(Vuelo("4X", 373, "HOU", 13, 15, "MSY", 15, 10, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)), List(Vuelo("4X", 214, "HOU", 9, 0, "MSY", 12, 40, 3), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0))))
  }

  test("test7") {
    val its7 = itsCurso40("DFW", "ATL")
    assert(its7.take(2) ==List(List(Vuelo("AA", 864, "DFW", 6, 56, "ATL", 15, 3, 0)), List(Vuelo("AA", 834, "DFW", 20, 0, "DCA", 13, 36, 0), Vuelo("AA", 319, "DCA", 15, 59, "ORD", 17, 10, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0))) )
  }

  test("test8") {
    val its8 = itsCurso100("PHX", "LAX")
    assert(its8.take(2) === List(List(Vuelo("DL", 588, "PHX", 12, 55, "DFW", 16, 15, 0), Vuelo("DL", 161, "DFW", 19, 10, "LAX", 10, 20, 0)), List(Vuelo("DL", 588, "PHX", 12, 55, "DFW", 16, 15, 0), Vuelo("DL", 139, "DFW", 18, 52, "LAX", 11, 20, 1))) )
  }

  test("test9") {
    val its9 = itsCurso100("PHX", "DTW")
    assert(its9.take(2) === List(List(Vuelo("DL", 296, "PHX", 14, 25, "ATL", 19, 53, 0), Vuelo("DL", 868, "ATL", 8, 26, "DTW", 10, 10, 0)), List(Vuelo("DL", 588, "PHX", 12, 55, "DFW", 16, 15, 0), Vuelo("DL", 722, "DFW", 16, 50, "ATL", 19, 44, 0), Vuelo("DL", 868, "ATL", 8, 26, "DTW", 10, 10, 0))) )
  }
}