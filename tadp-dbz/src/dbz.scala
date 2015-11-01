object TpDbz {
  
  case class Guerrero(
      nombre: String,
      tipo:Tipo,
      maximoKi: Int,
      items:List[Item]) {
  var ki = maximoKi
   
  def aumentarKi (numero: Int) {
        copy(maximoKi = Math.min( ki + numero,maximoKi))
      }
    
  }
  
  abstract class Tipo(val adicionales: List[Adicional]= List()) // adiocional --> cola, poder curativo ??
  
  case object Humano extends Tipo
  case object Saiyajin extends Tipo {
    var cola = true
    
  }
  case object Androide extends Tipo
  case object Namekusein extends Tipo
  case object Monstruo extends Tipo
  
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