package dbz

abstract class Tipo {
    
  def subirKi(g: Guerrero) = g.aumentarKi(100)
  
  def tieneCola = false
  def puedeFusionarse = false
}
  


case object Androide extends Tipo {
  override def subirKi(guerrero:Guerrero) = guerrero.aumentarKi(0)
}
  
case object Namekusein extends Tipo {
  override def puedeFusionarse = true
}
case object Monstruo extends Tipo
case object Humano extends Tipo {
  override def puedeFusionarse = true
}

case class Fusionado(original: Guerrero) extends Tipo