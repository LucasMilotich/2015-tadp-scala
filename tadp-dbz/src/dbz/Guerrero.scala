package dbz

import scala.util.Try
import scala.util.Failure
import scala.util.Success

case class Guerrero(
    nombre: String,
    tipo: Tipo,
    ki: Int,
    maximoKi: Int,
    items: Set[Item] = Set(),
    movimientosAprendidos: List[Movimiento] = List(),
    movimientosRobados: List[Movimiento] = List(),
    movimientosUtilizados: List[Movimiento] = List(),
    estado: Estado = NormalGuerrero) {

  def aumentarKi(numero: Int) =
    copy(ki = Math.min(ki + numero, maximoKi))

  def bajarKi(numero: Int) = {
    val nuevoGuerrero = copy(ki = Math.max(ki - numero, 0))
    if (nuevoGuerrero.ki == 0)
      nuevoGuerrero.morir
    else nuevoGuerrero
  }

  def bajarKiHasta(numero: Int) = {
    require(numero >= 0 && numero < ki, "Numero de Ki invalido para bajar")
    if (numero == 0) morir
    else copy(ki = numero)
  }

  def cargarKi =
    tipo.subirKi(this)

  def multiplicarMaximoKi(n: Int) =
    copy(maximoKi = maximoKi * n)

  def fusionar(compa: Guerrero) = {
    copy(tipo = Fusionado(this),
      maximoKi = maximoKi + compa.maximoKi,
      ki = ki + compa.ki,
      movimientosAprendidos = movimientosAprendidos ++ compa.movimientosAprendidos)
  }

  def usarItem(unItem: Item, oponente: Guerrero) = {
    unItem.aplicarEn(this, oponente)
  }

  def sacarItem(unItem: Item) = {
    val nuevosItems = items.-(unItem)
    copy(items = nuevosItems)
  }

  def agregarItem(unItem: Item) = {
    val nuevosItems = items.+(unItem)
    copy(items = nuevosItems)
  }

  def agregarItems(nuevosItems: Set[Item]) = {
    copy(items = items ++ nuevosItems)
  }

  def tieneItem(un_item: Item) =
    items.contains(un_item)

  def reemplazarItem(itemFuera: Item, itemDentro: Item) =
    sacarItem(itemFuera).agregarItem(itemDentro)

  def cambiarTipo(nuevoTipo: Tipo) =
    copy(tipo = nuevoTipo)

  def recibiExplosionDe(atacante: Guerrero) = {
    atacante.tipo match {
      case Androide => bajarKi(3 * atacante.ki)
      case _ => bajarKi(2 * atacante.ki)
    }
  }

  def explota = {
    bajarKi(ki)
  }

  def tieneCola = {
    tipo.tieneCola
  }

  def cambiarEstado(nuevoEstado: Estado) = {
    (tipo match {
      case Fusionado(original) => original
      case Saiyajin(forma, cola) if (nuevoEstado == Inconsciente || nuevoEstado == Muerto)
        => copy(tipo = Saiyajin(NormalSaiyajin, cola))
      case Androide if (nuevoEstado == Inconsciente)
        => throw new RuntimeException("Un androide no puede quedar inconsciente")
      case _ => this
    })
      .copy(estado = nuevoEstado)
  }

  def morir = {
    cambiarEstado(Muerto)
    throw new RuntimeException("Murio " + nombre)
  }

  def podesLanzarOnda(cantidad: Int) = {
    cantidad < ki
  }

  def recibirOnda(cantidad: Int) = {
    tipo match {
      case Monstruo(_) => bajarKi(cantidad / 2)
      case Androide => aumentarKi(cantidad)
      case _ => bajarKi(cantidad * 2)
    }
  }

  def comer(oponente: Guerrero) = {
    aprenderMovimientosDe(oponente)
  }

  def formaDeDigerir = {
    tipo match {
      case Monstruo(formaDeDigerir) => formaDeDigerir
      case _ => DigestionDefault
    }
  }

  def aprenderMovimiento(movimiento: Movimiento) =
    aprenderMovimientos(List(movimiento))

  def aprenderMovimientos(nuevosMovimientos: List[Movimiento]) =
    copy(movimientosAprendidos = movimientosAprendidos ++ nuevosMovimientos)

  def aprenderMovimientosDe(oponente: Guerrero) =
    copy(movimientosRobados = movimientosRobados ++ oponente.movimientosAprendidos)

  def limpiarMovimientosRobados =
    copy(movimientosRobados = List())

  def esferasCompletas = {
    (1 to 7).forall(x => this.tieneItem(EsferasDelDragon(x)))
  }

  def quitarEsferas = {
    copy(items = items.filterNot(_.sosEsfera))
  }

  def movimientoMasEfectivoContra(oponente: Guerrero)(unCriterio: Criterio): Option[Movimiento] = {
    val ms = movimientosAprendidos.filter { movimiento => movimiento.cuantificadoSegun(this, oponente)(unCriterio) > 0 }
    
    if (ms == List()) {
      None
    } else {
      Some(ms.maxBy { movimiento => movimiento.cuantificadoSegun(this, oponente)(unCriterio) })
    }
  }

  def pelearRound(movimiento: Movimiento)(oponente: Guerrero): Try[(Guerrero, Guerrero)] = {
    val combate: Try[(Guerrero, Guerrero)] = Try(movimiento.apply(this, oponente))
    val mayorVentaja = new Criterio({ (at, df) => at.ki - df.ki })
    val menorDesventaja = new Criterio({ (at, df) => (-1) / (at.ki - df.ki) })

    combate match {
      case Success((atacante, defensor)) => {
        var movimientoAutilizar: Option[Movimiento] = defensor.movimientoMasEfectivoContra(atacante)(mayorVentaja)
        if (movimientoAutilizar == None) movimientoAutilizar = defensor.movimientoMasEfectivoContra(atacante)(menorDesventaja)

        val contraataque = Try(movimientoAutilizar.get.apply(defensor, atacante))

        if (contraataque.isSuccess) Success(contraataque.get._2, contraataque.get._1)
        else contraataque
      }
      case Failure(_) => combate
    }

  }
  
  
  def planDeAtaqueContra(oponente: Guerrero, cantRounds: Int)(criterio: Criterio): List[Movimiento] = {
    var plan:List[Movimiento] = List()
    var combate:Try[(Guerrero,Guerrero)] = Success(this,oponente)
    
    for (i <- 1 to cantRounds) {
      val movimientoConveniente = combate.get._1.movimientoMasEfectivoContra(combate.get._2)(criterio)      
      
      movimientoConveniente match {
        case Some(movimiento:Movimiento) => {
                plan = plan.+:(movimiento)
                combate = combate.get._1.pelearRound(movimiento)(combate.get._2)               
        }
        case None => throw new RuntimeException("No encuentra plan de ataque")
      }
    }
    plan
  }
  
  
}
  