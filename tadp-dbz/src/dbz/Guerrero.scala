package dbz

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
    val (nuevoG, nuevoOp) = unItem.aplicarEn(this, oponente)

    (nuevoG, nuevoOp)
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

  def cambiarTipo(nuevoTipo: Tipo) = copy(tipo = nuevoTipo)

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
      case Saiyajin(forma, cola) if (nuevoEstado == Inconsciente || nuevoEstado == Muerto) => copy(tipo = Saiyajin(NormalSaiyajin, cola))
      case Androide if (nuevoEstado == Inconsciente) => throw new RuntimeException("Un androide no puede quedar inconsciente")

      case _ => this
    })
      .copy(estado = nuevoEstado)
  }

  def morir = {
    cambiarEstado(Muerto)
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
    this.aprenderMovimientosDe(oponente)
  }

  def formaDeDigerir = this.tipo.formaDeDigerir

  def aprenderMovimiento(movimiento: Movimiento) =
    copy(movimientosAprendidos = movimientosAprendidos ::: List(movimiento))

  def aprenderMovimientos(nuevosMovimientos: List[Movimiento]) =
    copy(movimientosAprendidos = movimientosAprendidos ++ nuevosMovimientos)

  def aprenderMovimientosDe(oponente: Guerrero) =
    copy(movimientosRobados = movimientosRobados ++ oponente.movimientosAprendidos)

  def limpiarMovimientosRobados =
    copy(movimientosRobados = List())

  def sosDelTipo(tipo: Tipo) = tipo match{
    case Monstruo(x) => this.tipo.equals(Monstruo(x))
    case _ => this.tipo.equals(tipo)
  }

  def esferasCompletas = {
    (1 to 7).forall(x => this.tieneItem(EsferasDelDragon(x)))
  }

  def quitarEsferas = {
    copy(items = this.items.filterNot(_.sosEsfera))
  }

  def movimientoMasEfectivoContra(oponente: Guerrero)(unCriterio: Criterio) = {
    //val ms = movimientosAprendidos.filter { movimiento => movimiento.cuantificadoSegun(this, oponente)(unCriterio) > 0 }
    //if (ms == List()) throw new RuntimeException("No conviene ningun movimiento a " + nombre)
    //ms.maxBy { movimiento => movimiento.cuantificadoSegun(this, oponente)(unCriterio) }
    movimientosAprendidos.maxBy { movimiento => movimiento.cuantificadoSegun(this, oponente)(unCriterio) }
  }
  
  def pelearRound(movimiento: Movimiento)(oponente: Guerrero) = {
    val (atacante,defensor) = movimiento.apply(this,oponente)
    val criterio = new Criterio({(at,df) => at.ki - df.ki})
    val movimientoAUtilizar = defensor.movimientoMasEfectivoContra(atacante)(criterio)
    val (d2,a2) = movimientoAUtilizar.apply(defensor,atacante)
    (a2,d2)
  }
  
  def planDeAtaqueContra(oponente: Guerrero, cantRounds: Int)(criterio: Criterio) = {
    var plan:List[Movimiento] = List()
    var combate:(Guerrero,Guerrero) = (this,oponente)
    
    for (i <- 1 to cantRounds) {
      val movimientoConveniente = combate._1.movimientoMasEfectivoContra(combate._2)(criterio)      
      plan = plan.+:(movimientoConveniente)
      
      combate = combate._1.pelearRound(movimientoConveniente)(combate._2)
    }
    plan
  }
  
  
}
  