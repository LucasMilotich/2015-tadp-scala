package test

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import dbz._

class MovimientosTest {

  val esferasCompletas: Set[Item] = Set(EsferasDelDragon(1), EsferasDelDragon(2), EsferasDelDragon(3), EsferasDelDragon(4), EsferasDelDragon(5), EsferasDelDragon(6), EsferasDelDragon(7))
  val krilin = new Guerrero("krilin", Humano, 350, 1500)
  val goku = new Guerrero("goku", Saiyajin(), 1500, 3000)
  val piccolo = new Guerrero("piccolo", Namekusein, 500, 1000)
  val a18 = new Guerrero("a18", Androide, 0, 0)
  val freezer = new Guerrero("freezer", Monstruo(DigestionDefault), 50, 800)
  val goku_ssj2 = new Guerrero("goku", Saiyajin(SuperSaiyajin(2)), 1500, 2000)

  val cell = new Guerrero("cell", Monstruo(SoloAndroides), 50, 800)
  val majinBuu = new Guerrero("majinBuu", Monstruo(SoloUltimoGuerrero), 50, 800)

  def assertKi(guerrero: Guerrero, cant: Int) = assertEquals(guerrero.ki, cant)

  @Test
  def `Guerrero_descansa_y_no_pasa_nada` = {
    val nuevoCombate = DejarseFajar(krilin, goku)
    assertEquals(nuevoCombate, (krilin, goku))
  }

  @Test
  def `Humano_carga_ki` = {
    val (cargaKi, elOtro) = CargarKi(krilin, freezer)

    assertKi(cargaKi, 450)
    assertKi(elOtro, 50)
  }

  @Test
  def `Androide_no_carga_ki` = {
    val (nuevo1, _) = CargarKi(a18, krilin)
    assertKi(nuevo1, 0)
  }

  @Test
  def `Saiyajin_carga_ki` = {
    val (nuevo1, nuevo2) = CargarKi(goku, piccolo)
    assertKi(nuevo1, 1600)
    assertKi(nuevo2, 500)
  }

  @Test
  def `SuperSaiyajin2_carga_300_ki` = {
    val (nuevo1, _) = CargarKi(goku_ssj2, krilin)
    assertKi(nuevo1, 1800)
  }

  @Test
  def `Fusion_Humano_Saiyajin_funciona` = {
    val (nuevoGuerrero, _) = FusionarCon(krilin)(goku, piccolo)

    assertKi(nuevoGuerrero, 1850)
    assertEquals(nuevoGuerrero.maximoKi, 4500)
  }

  @Test(expected = classOf[RuntimeException])
  def `Fusion_Saiyajin_Androide_lanza_error` = {
    FusionarCon(goku)(a18, krilin)
    fail()
  }

  @Test(expected = classOf[RuntimeException])
  def `krilin_no_puede_explotar_por_ser_humano` = {
    val excep = Explotar(goku, krilin)
    fail()
  }

  @Test
  def `explota_freezer_y_lastima_a_goku` = {
    val (nuevoFreezer, nuevoGoku) = Explotar(freezer, goku)

    assertKi(nuevoFreezer, 0)
    assertKi(nuevoGoku, 1400)
  }

  @Test
  def `humano_golpea_a_androide` = {
    val (humano, androide) = GolpesNinja(krilin, a18)

    assertKi(humano, 340)
    assertKi(androide, 0)
  }

  @Test
  def `atacante_con_menor_ki_le_pega_a_uno_con_mas_ki` = {
    val (menor, mayor) = GolpesNinja(krilin, piccolo)

    assertKi(menor, 330)
    assertKi(mayor, 500)
  }

  @Test
  def atacante_con_mayor_ki_le_pega_a_uno_con_menos_ki = {
    val (mayor, menor) = GolpesNinja(piccolo, krilin)

    assertKi(menor, 330)
    assertKi(mayor, 500)
  }

  @Test
  def `krilin_ataca_con_onda_10_a_freezer` = {
    val (nuevoKrilin, nuevoFreezer) = Onda(10)(krilin, freezer)

    assertKi(nuevoKrilin, 340)
    assertKi(nuevoFreezer, 45)
  }

  @Test(expected = classOf[RuntimeException])
  def `krilin_ataca_con_onda_1000_a_freezer_y_falla` = {
    Onda(1000)(krilin, freezer)
    fail()
  }

  @Test
  def krilin_come_oponente = {
    val (nuevoKrilin, nuevoFreezer) = comerOponente(krilin, freezer.aprenderMovimiento(CargarKi))

    assertEquals(nuevoKrilin.movimientosRobados.size, krilin.movimientosRobados.size)
    assertEquals(nuevoFreezer.estado, NormalGuerrero)
  }

  @Test
  def freezer_come_oponente = {
    val (nuevoFreezer, nuevoKrilin) = comerOponente(freezer, krilin.aprenderMovimiento(CargarKi))

    assertEquals(nuevoFreezer.movimientosRobados.size, nuevoKrilin.movimientosAprendidos.size)
    assertEquals(nuevoKrilin.estado, Muerto)
  }

  @Test
  def cell_come_androide = {
    val (nuevoCell, nuevoA18) = comerOponente(cell, a18.aprenderMovimiento(CargarKi))

    assertEquals(nuevoCell.movimientosRobados.size, nuevoA18.movimientosAprendidos.size)
    assertEquals(nuevoA18.estado, Muerto)
  }

  @Test
  def cell_come_no_androide = {
    val (nuevoCell, nuevoKrilin) = comerOponente(cell, krilin.aprenderMovimiento(CargarKi))

    assertEquals(nuevoCell.movimientosRobados.size, cell.movimientosRobados.size)
    assertEquals(nuevoKrilin.estado, NormalGuerrero)
  }

  @Test
  def majinBuu_come_oponente = {
    val (nuevoMajinBuu, _) = comerOponente(majinBuu, goku.aprenderMovimientos(List(DejarseFajar, DejarseFajar, DejarseFajar, CargarKi)))
    val (nuevoMajinBuu2, nuevoKrilin) = comerOponente(nuevoMajinBuu, krilin.aprenderMovimiento(CargarKi))

    assertEquals(nuevoMajinBuu2.movimientosRobados.size, nuevoKrilin.movimientosAprendidos.size)
    assertEquals(nuevoKrilin.estado, Muerto)
  }

  @Test
  def majinBuu_come_oponente2 = {
    val (nuevoMajinBuu, _) = comerOponente(majinBuu, goku.aprenderMovimiento(CargarKi))
    val (nuevoMajinBuu2, nuevoKrilin) = comerOponente(nuevoMajinBuu, krilin.aprenderMovimientos(List(DejarseFajar, DejarseFajar, DejarseFajar, CargarKi)))

    assertEquals(nuevoMajinBuu2.movimientosRobados.size, nuevoKrilin.movimientosAprendidos.size)
    assertEquals(nuevoKrilin.estado, Muerto)
  }

  @Test
  def goku_tiene_todas_las_esferas = {
    val nuevoGoku = goku.agregarItems(esferasCompletas)
    assertEquals(true, nuevoGoku.esferasCompletas)
  }

  @Test
  def goku_no_tiene_todas_las_esferas = {
    val items: Set[Item] = Set(EsferasDelDragon(1), EsferasDelDragon(2))
    val nuevoGoku = goku.agregarItems(items)

    assertEquals(false, nuevoGoku.esferasCompletas)
  }

  @Test
  def majinBuu_hace_Magia_con_esferas = {
    val (nuevoMajinBuu, nuevoGoku) = hacerMagia(Inconsciente, None)(majinBuu.agregarItems(esferasCompletas), goku)

    assertEquals(Inconsciente, nuevoMajinBuu.estado)
    assertEquals(Inconsciente, nuevoGoku.estado)
    assertEquals(false, nuevoMajinBuu.esferasCompletas)
  }
  
//  @Test
//  def majinBuu_hace_Magia_a_el_mismo = {
//    val (nuevoMajinBuu, nuevoGoku) = hacerMagia(Inconsciente, majinBuu)(majinBuu.agregarItems(esferasCompletas), goku)
//
//    assertEquals(Inconsciente, nuevoMajinBuu.estado)
//    assertEquals(Inconsciente, nuevoGoku.estado)
//    assertEquals(false, nuevoMajinBuu.esferasCompletas)
//  }

  @Test
  def majinBuu_hace_Magia_sin_esferas = {
    val (nuevoMajinBuu, nuevoGoku) = hacerMagia(Inconsciente, null)(majinBuu, goku)

    assertEquals(NormalGuerrero, nuevoMajinBuu.estado)
    assertEquals(NormalGuerrero, nuevoGoku.estado)
    assertEquals(false, nuevoMajinBuu.esferasCompletas)
  }

  @Test
  def krilin_no_puede_hacer_magia = {
    val (nuevoKrilin, nuevoGoku) = hacerMagia(Inconsciente, null)(krilin.agregarItems(esferasCompletas), goku)

    assertEquals(NormalGuerrero, nuevoKrilin.estado)
    assertEquals(NormalGuerrero, nuevoGoku.estado)
    assertEquals(true, nuevoKrilin.esferasCompletas)
  }

}
