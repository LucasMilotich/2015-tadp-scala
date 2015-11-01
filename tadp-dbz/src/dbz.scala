package object TpDbz {
  
  case class Guerrero(
      nombre: String,
      tipo:Tipo,
      maximoKi: Int,
      items:List[Item] = List()) {
  
    var ki = maximoKi
   
  def aumentarKi (numero: Int) = copy(maximoKi = Math.min(ki + numero, maximoKi))
      
    
  }
  
  abstract class Tipo
  
  case object Humano extends Tipo
  case object Saiyajin extends Tipo {
    var cola = true
    var nivel = 1
  }
  case object Androide extends Tipo
  case object Namekusein extends Tipo
  case object Monstruo extends Tipo
  
  abstract class Item(){ //implementar, solo para que no tire error
    
  }
 
  
 trait Movimiento extends Function2[Guerrero,Guerrero,Guerrero] {
   override def apply(guerrero: Guerrero, oponente: Guerrero): Guerrero
 }
 
  case object DejarseFajar extends Movimiento {
   override def apply(guerrero: Guerrero, oponente: Guerrero) = guerrero
   
 }
 
  case object CargarKi extends Movimiento {
    override def apply(guerrero: Guerrero, oponente: Guerrero) = {
      guerrero.tipo match {
        case Saiyajin => guerrero.aumentarKi(150)
        case Androide => guerrero.aumentarKi(0)
        case _ => guerrero.aumentarKi(100)
      }
      
    }
  }
  
}