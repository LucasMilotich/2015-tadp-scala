package Dbz

trait AccionComer {
  def comerseA (o: Guerrero)
}

class Monstruo extends Guerrero with AccionComer {
  def comerseA(oponente: Guerrero) {
    
  }
}

case object Cell extends Monstruo {
  def comerseA(oponente: Androide) {
    //overridea el comerseA -> agrega todos los movimientos de todos los ANDROIDES que se comio.
  }
  
}

case object MajinBuu extends Monstruo {
  override def comerseA(oponente: Guerrero) {
  //overridea el comerseA -> se agrega los movimientos del ultimo que se comio.
  }
}