package Dbz

class Saiyajin extends Guerrero {
  var cola = true
  var forma: FormaSaiyajin = new NormalSaiyajin
  //forma -> normal, mono, ss

  override def cargarKi = {
    forma.cargarKi(this)
  }
  
  def convertirseEnMono = {
    //debe tener cola y foto de luna entre items
    maximoKi = maximoKi*3
    //recupera todo el ki perdido...?
  }
  
  def convertirseEnSS = {
    forma.convertirSS(this)
  }
}

abstract class FormaSaiyajin {
  def cargarKi(guerrero: Guerrero) = {
    guerrero.subirKi(100)
  }
  
  def convertirSS(guerrero: Saiyajin) = {
    guerrero.forma = new SuperSaiyajin
    //falta subirle el ki
  }
}

class NormalSaiyajin extends FormaSaiyajin
class MonoSaiyajin extends FormaSaiyajin
class SuperSaiyajin extends FormaSaiyajin {
  var nivel:Int = 1
  
  override def cargarKi(guerrero: Guerrero) = {
    guerrero.subirKi(150)
  }
  
  override def convertirSS(guerrero: Saiyajin) = {
    nivel += 1
    guerrero.maximoKi = guerrero.maximoKi * nivel * 5
  }
}