package dbz

trait Movimiento extends Function2[Guerrero,Option[Guerrero],Guerrero] {
   override def apply(guerrero: Guerrero, oponente: Option[Guerrero]= None): Guerrero
 }
 
case object DejarseFajar extends Movimiento {
   override def apply(guerrero: Guerrero, op: Option[Guerrero] = None) = {
     println("pegame y decime " + guerrero.nombre)
     guerrero
 }
}
 
case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero, op: Option[Guerrero] = None) = {
      guerrero.cargarKi
    }
 }