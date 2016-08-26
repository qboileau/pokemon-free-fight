package pkm


import pkm.data.Pokedex
import pkm.models.{Arena, Trainer, Pokemon, RandomIA}

import scala.util.Random

object Main extends App {

  println(Pokedex.pokemons.mkString("\n"))

  val randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  println(s"Random: $randomPokemon")

  def generatePokemons(): Seq[Pokemon] = {
    (0 until 5).map(i => Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length)))
  }

  def gameFinished(arena: Arena) = arena.trainerA.isDead || arena.trainerB.isDead

  def prounceWinner(arena: Arena): Unit = {
    //TODO case when a and b ar dead
    if (arena.trainerA.isDead()) println(arena.trainerB.name+" wins")
    println(arena.trainerA.name+" wins")
  }

  def gameLoop(arena : Arena): Unit = {

    val newArena = arena.update()
    gameFinished(newArena) match {
      case true => prounceWinner(newArena)
      case false => gameLoop(newArena)
    }
  }

  val trainerA = Trainer("Red", 500, 0, generatePokemons(), new RandomIA)
  val trainerB = Trainer("Blue", 500, 0, generatePokemons(), new RandomIA)
  gameLoop(Arena(trainerA, trainerB))
}
