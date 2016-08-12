package pkm


import pkm.data.Pokedex
import pkm.models.{Arene, Dresseur, Pokemon, RandomIA}

import scala.util.Random

object Main extends App {

  println(Pokedex.pokemons.mkString("\n"))

  val randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  println(s"Random: $randomPokemon")

  def generatePokemons(): Seq[Pokemon] = {
    (0 until 5).map(i => Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length)))
  }

  val dresseurA = Dresseur(500, 0, generatePokemons(), new RandomIA)
  val dresseurB = Dresseur(500, 0, generatePokemons(), new RandomIA)


  val arene = new Arene(dresseurA, dresseurB)
  arene.gameLoop()



}
