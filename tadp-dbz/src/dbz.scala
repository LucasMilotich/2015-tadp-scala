

object TpDbz {
  
  case class Guerrero(val nombre: String, val items:List[Item], val bienEstar: Int,val tipo:Tipo){
   
  def aumentarKi{
        copy(bienEstar =Math.min( bienEstar + 100,500))
      }
    
  }
  
  abstract class Tipo(val adicionales: List[Adicional]= List()) // adiocional --> cola, poder curativo ??
  
  case object Humano extends Tipo
  case object Saiyajin extends Tipo
  case object Androide extends Tipo
  case object Namekusein extends Tipo
  case object Monstruo extends Tipo
  
  case class Adicional () //implementar, solo para que no tire error
  
  abstract class Item(){ //implementar, solo para que no tire error
    
  }
  
 type Movimiento = Function1[Guerrero, Guerrero]
 
 val dejarseFajar = (guerrero: Guerrero) => { // si se queda quieto devuelvo al mismo guerrero
   guerrero
 }
 
 val cargarKi = (guerrero : Guerrero) =>{
   val guerreroConMasKi = guerrero.copy()
   guerreroConMasKi.tipo match{
     case Androide => guerreroConMasKi
     case Saiyajin => //segun el estado aumentarlo
     case _ => guerrero.aumentarKi 
   }
   
 }
 
  
}