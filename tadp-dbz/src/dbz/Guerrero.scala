package dbz

case class Guerrero(
      nombre: String,
      tipo:Tipo,
      ki: Int,
      maximoKi: Int,
      items:List[Item] = List()) {
     
  def aumentarKi (numero: Int) =
    copy(ki = Math.min(ki + numero, maximoKi))
    
  def cargarKi = 
    tipo.subirKi(this)
  
  def tieneItem(un_item: Item) = 
   items.contains(un_item)
  
  def multiplicarMaximoKi(n :Int) =
    copy(maximoKi = maximoKi * n)
}
  
