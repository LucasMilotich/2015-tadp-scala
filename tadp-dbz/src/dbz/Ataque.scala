package dbz

abstract class TipoAtaque extends Movimiento {

}

case object Explotar extends TipoAtaque {
  def apply(guerrero: Guerrero, atacado: Guerrero) = {
    guerrero.tipo match {
      case Androide =>
        (guerrero.explota,
          atacado.recibiExplosionDe(guerrero))
      case Monstruo =>
        (guerrero.explota,
          atacado.recibiExplosionDe(guerrero))
      case _ => throw new RuntimeException("No podes explotar")

    }
  }
}

case object GolpesNinja extends TipoAtaque {
  def apply(guerrero: Guerrero, atacado: Guerrero) = {
    (guerrero.tipo, atacado.tipo) match {
      case (Humano, Androide) => (guerrero.bajarKi(10), atacado)
      case _ => if (guerrero.ki < atacado.ki) {
        (guerrero.bajarKi(20), atacado)

      } else (guerrero, atacado.bajarKi(20))
    }
  }
}

case class Onda(cantidad: Int) extends TipoAtaque {
  def apply(guerrero: Guerrero, atacado: Guerrero) = {
    if (!guerrero.podesLanzarOnda(cantidad)) {
      throw new RuntimeException("No tenes ki suficiente para lanzar la onda")
    }
    (guerrero.bajarKi(cantidad), atacado.recibirOnda(cantidad))

  }

  case class Genkidama(cantidad: Int) extends TipoAtaque {
    def apply(guerrero: Guerrero, atacado: Guerrero) = {
      //val cantidadAElevar= guerrero.vecesSeguidasQueTeDejasteFajar???
      (guerrero, atacado.bajarKi(cantidad))

    }
  }

}