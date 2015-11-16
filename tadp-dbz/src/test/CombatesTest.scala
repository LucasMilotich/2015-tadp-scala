package test

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import dbz._

class CombatesTest {
  
  /*nombre: String,
    tipo: Tipo,
    ki: Int,
    maximoKi: Int,
    items: Set[Item] = Set(),
    movimientosAprendidos: List[Movimiento] = List(),
  */
  
  val goku = new Guerrero("goku", Saiyajin(), 7000, 10000, Set(), List(GolpesNinja, Onda(300), Onda(5), Onda(20),Convertirse(SuperSaiyajin())))
  val goku_ssj2 = new Guerrero("goku", Saiyajin(SuperSaiyajin(2)), 15000, 20000,Set(semillaDelErmitanio,EsferasDelDragon(4)),List(GolpesNinja, Onda(300), Onda(5), Onda(20)))
  
  val krilin = new Guerrero("krilin", Humano, 3500, 4500,Set(),List(GolpesNinja,Onda(20)))
  val yajirobe = new Guerrero("yajirobe",Humano,1000,3000,Set(semillaDelErmitanio,arma(Filosa)),List(usarItem(arma(Filosa)),GolpesNinja))
  
  val freezer = new Guerrero("freezer", Monstruo(DigestionDefault), 15000, 18000)
  val celljr = new Guerrero("cell jr",Monstruo(DigestionDefault),700,2000,Set(semillaDelErmitanio),List(GolpesNinja,Onda(10),Onda(5),Onda(20)))
  
  val piccolo = new Guerrero("piccolo", Namekusein, 5000, 10000, Set(arma(Roma), semillaDelErmitanio), List(usarItem(arma(Roma)), Onda(250), Onda(300)))
  
  val a18 = new Guerrero("a18", Androide, 0, 0)  
  
  //////////
  val mayorVentaja = new Criterio({ (at, df) => at.ki - df.ki })
  val menorDesventaja = new Criterio({ (at, df) => 
    var n: Float = at.ki - df.ki
    n = (-1) / n
    n })
  //////////  
  
  @Test
  def `movimientoMasEfectivo_yajirobe_contra_krilin` = {
    
    val (atacante,defensor) = Onda(50)(krilin,yajirobe)
 
    assertFalse(defensor.movimientoMasEfectivoContra(atacante)(menorDesventaja).isEmpty)
    assertTrue(defensor.movimientoMasEfectivoContra(atacante)(mayorVentaja).isEmpty)
  }
  
  @Test
  def `pelea_round_guerrero2_tiene_mas_ki` = {
    val (atacante,defensor) = krilin.pelearRound(Onda(20))(yajirobe)
    
    assertEquals(atacante.ki,3471)
    assertEquals(defensor.ki,960)
  }
  
  
}