package dbz

case class Guerrero(
      nombre: String,
      tipo:Tipo,
      ki: Int,
      maximoKi: Int,
      items:Set[Item] = Set(),
      movimientos: List[Movimiento] = List(),
      estado:Estado = NormalGuerrero) {
          
  def aumentarKi (numero: Int) =
    copy(ki = Math.min(ki + numero, maximoKi))
    
  def bajarKi (numero: Int) = {
    val nuevoGuerrero = copy(ki = Math.max(ki-numero,0))
    if (nuevoGuerrero.ki == 0)
     nuevoGuerrero.die
    else nuevoGuerrero
  }
    
    
  def cargarKi = 
    tipo.subirKi(this)
  
  def tieneItem(un_item: Item) = 
   items.contains(un_item)
  
  def multiplicarMaximoKi(n :Int) =
    copy(maximoKi = maximoKi * n)
    
  def fusionar(compa:Guerrero) = {
    copy(tipo = Fusionado(this),
        maximoKi = maximoKi + compa.maximoKi,
        ki = ki + compa.ki,
        movimientos = movimientos ++ compa.movimientos)
  }
  
  def usarItem(unItem: Item) = {
    unItem.aplicarSobre(this)
    val nuevosItems = items.-(unItem)
    copy(items = nuevosItems)
  }
  
  def recibiExplosionDe(atacante:Guerrero)={
    atacante.tipo match{
      case Androide => bajarKi(3 * atacante.ki) 
      case _ => bajarKi(2 * atacante.ki)
    }
  }
  
  def explota  ={
    bajarKi(ki)
      }
 
  def tieneCola = {
    tipo.tieneCola
  }

 def cambiarEstado(nuevoEstado: Estado) = {
   (tipo match {
      case Fusionado(original) => original
      case Saiyajin(forma, cola) if (nuevoEstado == Inconsciente || nuevoEstado == Muerto) => copy(tipo = Saiyajin(NormalSaiyajin,cola))
      case Androide if (nuevoEstado == Inconsciente) => throw new RuntimeException("Un androide no puede quedar inconsciente")
      
     case _ => this
  })
    .copy(estado=nuevoEstado)
 }
  
  def die  = {
    cambiarEstado(Muerto)
  }
  
  def podesLanzarOnda(cantidad: Int) ={
    cantidad < ki
  }
  
  def recibirOnda(cantidad:Int) ={
      tipo match {
      case Monstruo => bajarKi(cantidad /2)
      case _ => bajarKi(cantidad*2)
      
    }
  }
}
  