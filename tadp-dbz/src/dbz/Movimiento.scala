package dbz

trait Movimiento extends Function2[Guerrero,Guerrero,Guerrero] {
   override def apply(guerrero: Guerrero, oponente: Guerrero): Guerrero
 }
 
  case object DejarseFajar extends Movimiento {
   override def apply(guerrero: Guerrero, oponente: Guerrero) = guerrero
   
 }
 
  case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero, oponente: Guerrero) = {
      guerrero.tipo.subirKi(guerrero)
    }
  }
