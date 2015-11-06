package dbz

abstract class Tipo(val formaDeDigerir: FormaDeDigerir = PasarVerguenza) {
    
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

case class Monstruo(override val formaDeDigerir: FormaDeDigerir) extends Tipo{
}

case object Humano extends Tipo {
  override def puedeFusionarse = true
}

case class Fusionado(original: Guerrero) extends Tipo


abstract class FormaDeDigerir

case object PasarVerguenza extends FormaDeDigerir
case object Default extends FormaDeDigerir
case object SoloAndroides extends FormaDeDigerir
case object SoloHumanos extends FormaDeDigerir
case object SoloUltimoGuerrero extends FormaDeDigerir