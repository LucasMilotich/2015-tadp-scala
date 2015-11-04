package dbz
 

trait Movimiento extends Function1[(Guerrero,Guerrero),(Guerrero,Guerrero)] {
   override def apply(guerrero1: Guerrero, guerrero2:Guerrero): (Guerrero,Guerrero)
 }
 
case object DejarseFajar extends Movimiento {
   override def apply(guerrero: Guerrero, g :Guerrero) = {
     println("pegame y decime " + guerrero.nombre)
     (guerrero,g)
 }
}
 
case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero, g :Guerrero) = {
      (guerrero.cargarKi,g)
    }
 }

case class FusionarCon(compa: Guerrero) extends Movimiento {
  override def apply(guerrero: Guerrero,g :Guerrero) = {
    if (guerrero.tipo.puedeFusionarse && compa.tipo.puedeFusionarse)
        (guerrero.fusionar(compa),g)
    else throw new RuntimeException("No se puede fusionar una de esas especies")
  }
  
}

//que valga para mono y para ssj al mismo tiempo
case class Convertirse(formaNueva: FormaSaiyajin) extends Movimiento {
  def apply(guerrero: Guerrero,g :Guerrero) = {
    formaNueva match {
      case SuperSaiyajin(_) if (guerrero.ki >= guerrero.maximoKi * 0.5) => 
        (Saiyajin().subirNivelSS(guerrero),g)  
      case MonoSaiyajin if (guerrero.tieneCola) =>
       (guerrero.multiplicarMaximoKi(3).copy(tipo = Saiyajin(MonoSaiyajin)),g)
      case _ =>
        (guerrero,g)
    }
  }
}

