package dbz

case class Guerrero(
      nombre: String,
      tipo:Tipo,
      maximoKi: Int,
      items:List[Item] = List()) {
  
  var ki = maximoKi
   
  def aumentarKi (numero: Int) = copy(maximoKi = Math.min(ki + numero, maximoKi))
      
}
  
