package test

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import dbz._

class CriteriosTest {
  
 val goku = new Guerrero("goku", Saiyajin(), 1500, 3000,Set(),List(GolpesNinja,Onda(300),Onda(5)))
 val piccolo = new Guerrero("piccolo", Namekusein, 500, 1000,Set(arma(Roma),semillaDelErmitanio),List(usarItem(arma(Roma)),Onda(250),Onda(300)))
 
 
 
 @Test
 def `goku_elige_movimiento_que_defensor_queda_con_mayor_ki`= {
   val criterio = new Criterio({(atacante,defensor) => defensor.ki})
   val movi = goku.movimientoMasEfectivoContra(piccolo)(criterio)
   assertEquals(movi,Onda(5))
 }
}