package Dbz

import scala.collection.mutable.Set

class Guerrero {
  var items: Set[Item] = Set()
  var ki: Int = 0
  var nombre: String = ""
  var maximoKi: Int = 0

  def dejarseFajar = {
    "pegame y decime " + nombre
  }
  
  def subirKi(numero: Int) = {
    ki = List(ki + numero,maximoKi).min
  }
  
  def cargarKi = {
    subirKi(100)
  }

  def usarItem(unItem: Item) {
    //afecta al enemigo...
    
    items.remove(unItem)
  }
  
}
