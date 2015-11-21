package test

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import dbz._

class CriteriosTest {

  
  val goku = new Guerrero("goku", Saiyajin(), 7000, 10000, Set(), List(GolpesNinja, Onda(300), Onda(5),Convertirse(SuperSaiyajin())))
  val piccolo = new Guerrero("piccolo", Namekusein, 5000, 10000, Set(arma(Roma), semillaDelErmitanio), List(usarItem(arma(Roma)), Onda(250), Onda(300)))
  val krilin = new Guerrero("krilin", Humano, 3500, 4500,Set(),List(GolpesNinja,Onda(20)))
  val a18 = new Guerrero("a18", Androide, 0, 0)
  val freezer = new Guerrero("freezer", Monstruo(DigestionDefault), 15000, 18000)
  val goku_ssj2 = new Guerrero("goku", Saiyajin(SuperSaiyajin(2)), 15000, 20000,Set(semillaDelErmitanio,EsferasDelDragon(4)),List())
  val yajirobe = new Guerrero("yajirobe",Humano,1000,4000,Set(semillaDelErmitanio,arma(Filosa)),List(usarItem(semillaDelErmitanio),usarItem(arma(Filosa)),GolpesNinja))
  val celljr = new Guerrero("cell jr",Monstruo(DigestionDefault),700,2000,Set(semillaDelErmitanio),List(GolpesNinja,Onda(10),Onda(5),Onda(20)))
  
 @Test
 def `krilin_elige_movimiento_que_defensor_queda_con_mayor_ki`= {
   val criterio = new Criterio({(atacante,defensor) => defensor.ki})
   val movi = krilin.movimientoMasEfectivoContra(piccolo)(criterio)
   assertEquals(movi.get,GolpesNinja)
 }
 
 @Test
 def `yajirobe_usa_plan_de_ataque_2_rounds_contra_freezer` = {
   val criterio = new Criterio({(atacante,_) => atacante.ki})
   val planDeAtaque = yajirobe.planDeAtaqueContra(celljr, 2)(criterio)
   
   assertEquals(planDeAtaque.size,(List(usarItem(arma(Filosa)),usarItem(semillaDelErmitanio))).size)
   
 }
 
}