package dbz

class Item {
  
}

case object semillaDelErmitaño extends Item
case object fotoDeLaLuna extends Item
case class esferaDelDragon(n:Int) extends Item {
  require(n >= 1 && n<=7)
}
