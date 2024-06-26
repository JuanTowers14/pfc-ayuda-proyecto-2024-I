package proyecto

import common._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure}

class ItinerarioPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    // Recibe una lista de vuelos y aeropuertos
    // Retorna una función que recibe los códigos de dos aeropuertos
    // Retorna todos los itinerarios posibles de cod1 a cod2
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

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val buscarItinerarios = itinerariosPar(vuelos, aeropuertos)

    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
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

    def calcularTiempoTotal(ruta: List[Vuelo]): Future[Int] = Future {
      val tiemposDeVueloFut = Future.sequence(ruta.map(vuelo => Future(calcularDuracionVuelo(vuelo))))
      val tiemposDeEsperaFut = Future.sequence(ruta.zip(ruta.tail).map { case (v1, v2) => Future(calcularTiempoEspera(v1, v2)) })

      val tiemposDeVuelo = Await.result(tiemposDeVueloFut, Duration.Inf)
      val tiemposDeEspera = Await.result(tiemposDeEsperaFut, Duration.Inf)

      tiemposDeVuelo.sum + tiemposDeEspera.sum
    }

    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val itinerarios = buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      val itinerariosConTiempoFut = Future.sequence(itinerarios.map { ruta =>
        calcularTiempoTotal(ruta).map(tiempo => (ruta, tiempo))
      })

      val itinerariosConTiempo = Await.result(itinerariosConTiempoFut, Duration.Inf)
      itinerariosConTiempo.sortBy(_._2).take(3).map(_._1)
    }
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val buscarItinerarios = itinerariosPar(vuelos, aeropuertos)

    def calcularEscalas(ruta: List[Vuelo]): Int = {
      val escalasIndividuales = ruta.map(_.Esc).sum
      val transiciones = ruta.size - 1
      escalasIndividuales + transiciones
    }
    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val futureItinerarios = Future {
        buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      }
      val futureResultados = futureItinerarios.flatMap { itinerarios =>
        val futureRutas = itinerarios.map { ruta =>
          Future {
            (ruta, calcularEscalas(ruta))
          }
        }
        Future.sequence(futureRutas).map { rutasConEscalas =>
          rutasConEscalas.sortBy(_._2).take(3).map(_._1)
        }
      }
      Await.result(futureResultados, Duration.Inf)
    }
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[vuelos] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    val buscarItinerarios = itinerariosPar(vuelos, aeropuertos)

    def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotalDeVuelo(ruta: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
      ruta.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos)).sum
    }
    (aeropuertoOrigen: String, aeropuertoDestino: String) => {
      val futureItinerarios = Future {
        buscarItinerarios(aeropuertoOrigen, aeropuertoDestino)
      }
      val futureResultados = futureItinerarios.flatMap { itinerarios =>
        val futureRutas = itinerarios.map { ruta =>
          Future {
            (ruta, calcularTiempoTotalDeVuelo(ruta, aeropuertos))
          }
        }
        Future.sequence(futureRutas).map { rutasConDuracion =>
          rutasConDuracion.sortBy(_._2).take(3).map(_._1)
        }
      }
      Await.result(futureResultados, Duration.Inf)
    }
  }

  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    val buscarItinerariosFn = itinerariosPar(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def calcularHoraLlegadaTotal(itinerario: List[Vuelo]): Int = {
      convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
    }

    def calcularHoraSalidaTotal(itinerario: List[Vuelo]): Int = {
      convertirAMinutos(itinerario.head.HS, itinerario.head.MS)
    }

    def calcularLapsoTiempo(horaLlegada: Int, horaCita: Int): Int = {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    def esValido(itinerario: List[Vuelo], tiempoCita: Int): Boolean = {
      val horaLlegada = calcularHoraLlegadaTotal(itinerario)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerariosFuturo = Future { buscarItinerariosFn(origen, destino) }

      val itinerariosValidosFuturo = todosItinerariosFuturo.flatMap { todosItinerarios =>
        Future.sequence(todosItinerarios.map(it => Future {
          if (esValido(it, tiempoCita)) Some(it) else None
        })).map(_.flatten)
      }

      val itinerariosOrdenadosFuturo = itinerariosValidosFuturo.flatMap { itinerariosValidos =>
        Future.sequence(itinerariosValidos.map { it =>
          Future {
            val horaLlegada = calcularHoraLlegadaTotal(it)
            val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
            (it, lapsoTiempo, calcularHoraSalidaTotal(it))
          }
        })
      }.map { itinerariosOrdenados =>
        itinerariosOrdenados.sortBy { case (_, lapsoTiempo, horaSalida) =>
          (lapsoTiempo, horaSalida)
        }
      }

      Await.result(itinerariosOrdenadosFuturo.map(_.headOption.map(_._1).getOrElse(List.empty)), Duration.Inf)
    }
  }

}
