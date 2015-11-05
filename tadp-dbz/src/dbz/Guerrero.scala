package dbz

case class Guerrero(
    nombre: String,
    tipo: Tipo,
    ki: Int,
    maximoKi: Int,
    items: Set[Item] = Set(),
    movimientosAprendidos: List[Movimiento] = List(),
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
    require(numero >= 0 && numero < ki, "Numero de Ki inválido para bajar")
    if (numero == 0) morir
    else copy(ki = numero)
  }

  def cargarKi =
    tipo.subirKi(this)

  def tieneItem(un_item: Item) =
    items.contains(un_item)

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

  def formaDeDigerir = this.tipo.formaDeDigerir

  def aprenderMovimiento(movimiento: Movimiento) =
    copy(movimientosAprendidos = movimientosAprendidos ::: List(movimiento))

  def aprenderMovimientos(nuevosMovimientos: List[Movimiento]) =
    copy(movimientosAprendidos = movimientosAprendidos ++ nuevosMovimientos)

  def aprenderMovimientosDe(oponente: Guerrero) =
    this.aprenderMovimientos(oponente.movimientosAprendidos)
}
  