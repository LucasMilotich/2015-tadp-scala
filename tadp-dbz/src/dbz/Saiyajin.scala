package dbz

case class Saiyajin(forma:FormaSaiyajin = NormalSaiyajin,
    cola:Boolean= true) extends Tipo {
  
  
  override def subirKi(guerrero:Guerrero) =
      forma.subirKi(guerrero)  
    
  def subirNivelSS(guerrero:Guerrero) =
    forma.subirNivel(guerrero)
  
  override def puedeFusionarse = true
    
  override def tieneCola = cola
}

abstract class FormaSaiyajin {
  
  def subirKi(g: Guerrero):Guerrero = g.aumentarKi(100)
  
  def subirNivel(g: Guerrero): Guerrero = {
    g.multiplicarMaximoKi(5)
      .copy(tipo = Saiyajin(SuperSaiyajin()))
  }
  
}

case object NormalSaiyajin extends FormaSaiyajin
case object MonoSaiyajin extends FormaSaiyajin
case class SuperSaiyajin(var nivel:Int = 1) extends FormaSaiyajin {
  
  override def subirKi(g: Guerrero) = 
    g.aumentarKi(150 * nivel)
 
  override def subirNivel(g: Guerrero) = {
    g.multiplicarMaximoKi(5 * nivel)
      .copy(tipo = Saiyajin(SuperSaiyajin(nivel+1)))
  }
}
