package dbz

abstract class TipoAtaque extends Movimiento {

}

case class Explotar(atacado: Guerrero) extends TipoAtaque {
  def apply(guerrero: Guerrero) = {
    guerrero.tipo match {
      case Androide =>
        guerrero.explota
        atacado.recibiExplosionDe(guerrero)
      case Monstruo =>
        guerrero.explota
        atacado.recibiExplosionDe(guerrero)
      case _ => throw new RuntimeException("No podes explotar")

    }
  }
}

case class GolpesNinja(atacado: Guerrero) extends TipoAtaque {
  def apply(guerrero: Guerrero) = {
    (guerrero.tipo, atacado.tipo) match {
      case (Humano, Androide) => guerrero.bajarKi(10)
      case _ => if (guerrero.ki < atacado.ki) {
        guerrero.bajarKi(20)

      } else atacado.bajarKi(20)
    }
  }
}