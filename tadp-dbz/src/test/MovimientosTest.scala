package test

import org.junit.Assert._
import org.junit.Test
import dbz._
import dbz.GolpesNinja
import dbz.Explotar

class MovimientosTest {

  val krilin = new Guerrero("krilin", Humano, 350, 1500)
  val goku = new Guerrero("goku", Saiyajin(), 1500, 3000)
  val piccolo = new Guerrero ("piccolo", Namekusein, 500, 1000)
  val a18 = new Guerrero("a18", Androide, 0, 0)
  val freezer= new Guerrero("freezer", Monstruo, 50,800)
  val goku_ssj2 = new Guerrero("goku", Saiyajin(SuperSaiyajin(2)), 1500, 2000)

  @Test
  def `Guerrero_descansa_y_no_pasa_nada` = {
    assertEquals(DejarseFajar(krilin), krilin)
  }

  @Test
  def `Humano_carga_ki` = {
    assertEquals(CargarKi(krilin).ki, 450)
  }

  @Test
  def `Androide_carga_ki` = {
    assertEquals(CargarKi(a18).ki, 0)
  }

  @Test
  def `Saiyajin_carga_ki` = {
    assertEquals(CargarKi(goku).ki, 1600)
  }

  @Test
  def `SuperSaiyajin2_carga_300_ki` = {
    assertEquals(CargarKi(goku_ssj2).ki, 1800)
  }

  @Test
  def `Fusion_Humano_Saiyajin_funciona` = {
    val nuevoGuerrero = FusionarCon(krilin)(goku)
    assertEquals(nuevoGuerrero.ki, 1850)
    assertEquals(nuevoGuerrero.maximoKi, 4500)
  }

  @Test //no me acuerdo como se testeaba el error
  def `Fusion_Saiyajin_Androide_lanza_error` = {
    FusionarCon(a18)(goku)
    fail()
  }

  @Test
  def `krilin no puede explotar por ser humano` = {
    val nuevoGoku = Explotar(goku)(krilin)
    fail()
  }

  
  @Test
  def `explota freezer y lastima a goku` = {
    val nuevoGoku = Explotar(goku)(freezer)
    assertEquals(nuevoGoku.ki,1400)
      }
  
  @Test
   def `humano golpea a andriode` = {
  val perdedor= GolpesNinja(a18)(krilin)
  assertEquals(perdedor.ki, 340)
    
  }
  
  @Test
  def ` atacante con menor ki le pega a uno con mas ki` ={
    val perdedor= GolpesNinja(piccolo)(krilin)
     assertEquals(perdedor.ki, 330)
    
  }
  
  @Test
  def `atacante con mayor ki le pega a uno con menos ki` ={
    val perdedor= GolpesNinja(krilin)(piccolo)
     assertEquals(perdedor.ki, 330)
    
  }
  
  @Test //pasa lo mismo que en el test de explotar, no puedo testear al otro guerrero
  def `krilin ataca a freezer` ={
    val nuevoKrilin= Onda(freezer, 10)(krilin)
    assertEquals(nuevoKrilin.ki, 340)
    
  }
  
  @Test
  def `krilin queire atacar a freezaar con mas de su ki` ={
    val nuevoKrilin= Onda(freezer, 1000)(krilin)
   fail()
    
  }
  
  
}
