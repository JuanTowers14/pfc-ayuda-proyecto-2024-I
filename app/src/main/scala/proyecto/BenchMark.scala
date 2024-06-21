package proyecto
import datos._
import org.scalameter._

import org.scalameter.{Key, Warmer, withWarmer}


class BenchMark {

  val objSecuencial = new Itinerario()
  val objParalelo = new ItinerarioPar()

  def itinerariosBenchMark(): Unit = {
    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA", 19,20)
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA",18, 53)*/
      objSecuencial.itinerarios(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objParalelo.itinerariosPar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba de Itinerarios con Lista de Vuelos C1 y Lista Aeropuertos, Buscando El Vuelo de PHX a LAX")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosTiempoBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objSecuencial.itinerariosTiempo(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 100,
      Key.exec.maxWarmupRuns := 100,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objParalelo.itinerariosTiempoPar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba de ItinerariosTiempo con Lista de Vuelos B1 y Lista Aeropuertos, Buscando El Vuelo de PHX a LAX")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosEscalasBenchMark(): Unit = {
    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objSecuencial.itinerariosEscalas(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objParalelo.itinerariosEscalasPar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba de ItinerariosEscalas con Lista de Vuelos C1 y Lista Aeropuertos, Buscando El Vuelo de PHX a LAX")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosAireBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objSecuencial.itinerariosAire(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      /*
      objParalelo.itinerariosSalidaPar(vuelosA1, aeropuertos)("HOU", "BNA")
      objParalelo.itinerariosSalidaPar(vuelosB1, aeropuertos)("DEN", "MIA")
      objSecuencial.itinerariosTiempo(vuelosD1, aeropuertos)("PHX", "LAX")
      */
      objParalelo.itinerariosAirePar(vuelosC1, aeropuertos)("PHX", "LAX")
    }

    println("Prueba de ItinerariosAire con Lista de Vuelos C1 y Lista Aeropuertos, Buscando El Vuelo de PHX a LAX")
    println("Secuencial: " + timeSecuencial)
    println("Paralela: " + timeParalela)

  }

  def itinerariosSalidaBenchMark(): Unit = {

    val timeSecuencial = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("DFW", "ATL", 15, 10)
      objSecuencial.itinerariosSalida(vuelosC1, aeropuertos)("ATL", "DFW", 15, 10)
    }

    val timeParalela = config(
      Key.exec.minWarmupRuns := 500,
      Key.exec.maxWarmupRuns := 500,
      Key.exec.benchRuns := 20,
      Key.verbose := false,
    ) withWarmer new Warmer.Default measure {
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("DFW", "ATL", 15, 10)
      objParalelo.itinerariosSalidaPar(vuelosC1, aeropuertos)("ATL", "DFW", 15, 10)
    }

    println("Prueba de ItinerariosAire con Lista de Vuelos C1 y Lista Aeropuertos, Buscando El Vuelo de ATL hasta DFW con una cita a las 15:10 ")
    println("Secuencial: " + timeParalela)
    println("Paralela: " + timeSecuencial)

  }

}