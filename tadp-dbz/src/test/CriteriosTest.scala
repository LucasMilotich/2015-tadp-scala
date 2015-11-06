package test

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import dbz._

class CriteriosTest {
  
 val cr = new Criterio({(atacante,defensor) => defensor.ki})
 val goku = new Guerrero("goku", Saiyajin(), 1500, 3000,Set(),List(GolpesNinja,Onda(300),Onda(5)))
 val piccolo = new Guerrero("piccolo", Namekusein, 500, 1000)

 
 
 @Test
 def `goku_elige_movimiento_que_defensor_queda_con_mayor_ki`= {
   val movi = goku.movimientoMasEfectivoContra(piccolo,cr)
   assertEquals(movi,Onda(5))
 }
}