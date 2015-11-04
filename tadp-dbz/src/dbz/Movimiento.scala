package dbz

trait Movimiento extends Function1[Guerrero,Guerrero] {
   override def apply(guerrero: Guerrero): Guerrero
 }
 
case object DejarseFajar extends Movimiento {
   override def apply(guerrero: Guerrero) = {
     println("pegame y decime " + guerrero.nombre)
     guerrero
 }
}
 
case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero) = {
      guerrero.cargarKi
    }
 }

case class FusionarCon(compa: Guerrero) extends Movimiento {
  override def apply(guerrero: Guerrero) = {
    if (guerrero.tipo.puedeFusionarse && compa.tipo.puedeFusionarse)
        guerrero.fusionar(compa)
    else throw new RuntimeException("No se puede fusionar una de esas especies")
  }
  
}

//que valga para mono y para ssj al mismo tiempo
case class Convertirse(formaNueva: FormaSaiyajin) extends Movimiento {
  def apply(guerrero: Guerrero) = {
    formaNueva match {
      case SuperSaiyajin(_) if (guerrero.ki >= guerrero.maximoKi * 0.5) => 
        Saiyajin().subirNivelSS(guerrero)  
      case MonoSaiyajin if (guerrero.tieneCola) =>
        guerrero.multiplicarMaximoKi(3).copy(tipo = Saiyajin(MonoSaiyajin))
      case _ =>
        guerrero
    }
  }
}

case class Explotar(atacado:Guerrero) extends Movimiento {
  def apply(guerrero:Guerrero) ={
    guerrero.explota
    atacado.recibiExplosionDe(guerrero)
  }
  
}