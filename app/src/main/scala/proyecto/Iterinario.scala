package proyecto

import scala.annotation.tailrec
import scala.collection.immutable.List

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]


  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def encontrarRutas(origen: String, destino: String, visitados: Set[String], rutaActual: List[Vuelo]): List[vuelos] = {
      if (origen == destino) {
        List(rutaActual)
      } else {
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          encontrarRutas(vuelo.Dst, destino, visitados + origen, rutaActual :+ vuelo)
        }
      }
    }

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      encontrarRutas(aeropuertoOrigen, aeropuertoDestino, Set(), List())
    }
  }

  def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
    val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
    val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

    val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
    val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

    val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
    val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

    val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

    if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
  }

  def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {

    val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
    val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS

    val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

    if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
  }

  def calcularTiempoTotal(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    val tiemposDeVuelo = ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos))
    val tiemposDeEspera = ruta.zip(ruta.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
    tiemposDeVuelo.sum + tiemposDeEspera.sum
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(ruta => calcularTiempoTotal(ruta, aeropuertos)).take(3)
    }
  }

  def calcularEscalas(ruta: List[Vuelo]): Int = {
    val escalasIndividuales = ruta.map(_.Esc).sum
    val transiciones = ruta.size - 1
    escalasIndividuales + transiciones
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas

    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(calcularEscalas).take(3)
    }
  }

  def calcularTiempoTotalDeVuelo(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos)).sum
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      buscarItinerarios(aeropuertoOrigen, aeropuertoDestino).sortBy(ruta => calcularTiempoTotalDeVuelo(ruta, aeropuertos)).take(3)
    }
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
    val buscarItinerarios = itinerarios(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = hora * 60 + minutos

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)

      val itinerariosGenerales = buscarItinerarios(origen, destino)

      val itinerariosFiltrados = itinerariosGenerales.filter { ruta =>
        val ultimaLlegada = ruta.last
        convertirAMinutos(ultimaLlegada.HL, ultimaLlegada.ML) <= tiempoCita &&
          ruta.forall(vuelo => convertirAMinutos(vuelo.HS, vuelo.MS) < tiempoCita)
      }

      if (itinerariosFiltrados.isEmpty) List()
      else {
        val salidaMasTarde = itinerariosFiltrados.map(it => convertirAMinutos(it.last.HS, it.last.MS)).max
        itinerariosFiltrados.filter(it => convertirAMinutos(it.last.HS, it.last.MS) == salidaMasTarde)
      }
    }
  }

}