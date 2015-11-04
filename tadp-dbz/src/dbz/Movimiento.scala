package dbz
 

trait Movimiento extends Function2[Guerrero,Guerrero,(Guerrero,Guerrero)] {
   override def apply(guerrero: Guerrero, oponente: Guerrero): (Guerrero,Guerrero)
 }
 
case object DejarseFajar extends Movimiento {
   override def apply (guerrero: Guerrero, oponente: Guerrero) = {
     println("pegame y decime " + guerrero.nombre)
     (guerrero,oponente)
 }
}
 
case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero, oponente :Guerrero) = {
      (guerrero.cargarKi,oponente)
    }
 }

case class FusionarCon(compa: Guerrero) extends Movimiento {
  override def apply(guerrero: Guerrero, oponente :Guerrero) = {
    if (guerrero.tipo.puedeFusionarse && compa.tipo.puedeFusionarse)
        (guerrero.fusionar(compa),oponente)
    else throw new RuntimeException("No se puede fusionar una de esas especies")
  }
  
}

//que valga para mono y para ssj al mismo tiempo
case class Convertirse(formaNueva: FormaSaiyajin) extends Movimiento {
  def apply(guerrero: Guerrero, oponente :Guerrero) = {
    (formaNueva match {
      case SuperSaiyajin(_) if (guerrero.ki >= guerrero.maximoKi * 0.5)
        => Saiyajin().subirNivelSS(guerrero)  
      case MonoSaiyajin if (guerrero.tieneCola)
        => guerrero.multiplicarMaximoKi(3).copy(tipo = Saiyajin(MonoSaiyajin))
      case _ => guerrero
    },oponente)
  }
}

