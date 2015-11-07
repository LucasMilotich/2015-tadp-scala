package dbz

abstract class Item {
  def aplicarEn(guerrero: Guerrero, oponente: Guerrero): (Guerrero, Guerrero) = (guerrero, oponente)
  
  def sosEsfera = false
}

case object semillaDelErmitanio extends Item {
  override def aplicarEn(guerrero: Guerrero, oponente: Guerrero) = {
    (guerrero.copy(ki = guerrero.maximoKi), oponente)
  }
}

case object fotoDeLaLuna extends Item

case class EsferasDelDragon(cantidad: Int) extends Item {
  override def sosEsfera = true
}

case class arma(tipo: TipoArma) extends Item {
  override def aplicarEn(guerrero: Guerrero, oponente: Guerrero) = {
    tipo.afectar(guerrero, oponente)
  }
}

abstract class TipoArma {
  def afectar(g: Guerrero, atacado: Guerrero): (Guerrero, Guerrero)
}

case object Roma extends TipoArma {
  override def afectar(guerrero: Guerrero, atacado: Guerrero) = {
    // en cambiarEstado ya se valida que no se androide para dejarlo inconsciente
    (guerrero,
      if (atacado.ki < 300) {
        atacado.cambiarEstado(Inconsciente)
      } else {
        atacado
      })
  }
}

case object Filosa extends TipoArma {
  override def afectar(atacante: Guerrero, atacado: Guerrero) = {
    val nuevoAtacado: Guerrero = atacado.bajarKi(atacante.ki / 100)

    (atacante,
      if (nuevoAtacado.tieneCola) {
        nuevoAtacado.bajarKiHasta(1).tipo match {
          case Saiyajin(MonoSaiyajin, true) => nuevoAtacado.cambiarTipo(Saiyajin(NormalSaiyajin, false)).cambiarEstado(Inconsciente)
          case Saiyajin(forma, true) => nuevoAtacado.cambiarTipo(Saiyajin(forma, false))
          case _ => nuevoAtacado
        }
      } else nuevoAtacado)
  }
}

case class Fuego(municiones: Int) extends TipoArma {
  override def afectar(atacante: Guerrero, atacado: Guerrero) = {
    (bajarMunicion(atacante),
      atacado.tipo match {
        case Humano => atacado.bajarKi(20)
        case Namekusein if (atacado.estado == Inconsciente) => atacado.bajarKi(10)
        case _ => atacado
      })
  }

  def bajarMunicion(guerrero: Guerrero) = {
    if (municiones > 0)
      guerrero.reemplazarItem(arma(Fuego(municiones)), arma(Fuego(municiones - 1)))
    else
      guerrero
  }
}
case class usarItem(unItem: Item) extends Movimiento {
  def apply(guerrero: Guerrero, oponente: Guerrero) = {
    if (guerrero.tieneItem(unItem))
      unItem.aplicarEn(guerrero, oponente)
    else
      (guerrero, oponente)
  }
}