package dbz

abstract class Tipo {
    
  def subirKi(g: Guerrero) = g.aumentarKi(100)
    
}
  


case object Androide extends Tipo {
  
  override def subirKi(guerrero:Guerrero) = guerrero.aumentarKi(0)
    
}
  
case object Namekusein extends Tipo
case object Monstruo extends Tipo
case object Humano extends Tipo
