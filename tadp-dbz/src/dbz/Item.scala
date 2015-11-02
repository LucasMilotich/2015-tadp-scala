package dbz

abstract class Item {
  def aplicarSobre(g: Guerrero): Guerrero = g
}

case object semillaDelErmitaño extends Item {
  override def aplicarSobre(g: Guerrero) = {
    g.copy(ki = g.maximoKi)
  }
}
case object fotoDeLaLuna extends Item {
  
}

case class esferaDelDragon(n:Int) extends Item {
  require(n >= 1 && n<=7)
  
}

