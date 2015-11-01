package dbz

case object Saiyajin extends Tipo {
    var cola:Boolean = true
    var forma:FormaSaiyajin = NormalSaiyajin
    
    override def subirKi(guerrero:Guerrero) = forma.subirKi(guerrero)
    
}

abstract class FormaSaiyajin {
  
  def subirKi(g: Guerrero):Guerrero = g.aumentarKi(100)
  
}

case object NormalSaiyajin extends FormaSaiyajin
case object MonoSaiyajin extends FormaSaiyajin
case class SuperSaiyajin(var nivel:Int = 1) extends FormaSaiyajin {
  
  override def subirKi(g: Guerrero) = g.aumentarKi(150 * nivel)
  
}
