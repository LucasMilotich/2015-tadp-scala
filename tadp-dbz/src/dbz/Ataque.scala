package dbz



abstract class TipoAtaque extends Movimiento{
  
}


case class Explotar(atacado:Guerrero){
  def apply(guerrero:Guerrero) ={
    guerrero.explota
    atacado.recibiExplosionDe(guerrero)
  }
}

case class GolpesNinja(atacado:Guerrero){
  def apply(guerrero:Guerrero) ={
    (guerrero.tipo, atacado.tipo) match{
      case (Humano, Androide)=> guerrero.bajarKi(10)
      case _ => if (guerrero.ki < atacado.ki){
        guerrero.bajarKi(20)
        
      }
      else atacado.bajarKi(20)
      }
    }
}