package dbz

class Criterio (funcion: (Guerrero,Guerrero)=>Int) {
  def analizar(atacante: Guerrero, defensor: Guerrero) = {
    funcion(atacante,defensor)
  }
}