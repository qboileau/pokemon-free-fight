package pkm.models

/**
 * Created by quentin on 12/08/16.
 */
class Arene(dresseurA: Dresseur, dresseurB: Dresseur) {

  def gameLoop(): Unit = {

    val pokemonA = dresseurA.firstAlivePokemon()
    val pokemonB = dresseurB.firstAlivePokemon()

    val actionA = dresseurA.action()
    val actionB = dresseurB.action()

    def update(victim: Dresseur, victimAction: Action,  bullyAction: Action): Dresseur = {
      val pokemon = victim.firstAlivePokemon()
      bullyAction match {
        case AtkDresseur(atk) => atk match {
          case BasicAttack(_, _, dmg) => victim.copy(life = victim.life - dmg)
          case PowerAttack(_, _, dmg, cost) => victim.copy(life = victim.life - dmg)
        }
        case AtkPokemon(atk) => atk match {
          case BasicAttack(_, _, dmg) =>  victim.copy(pokemons = victim.pokemons.map(p => if (p.number = pokemon.)))
          case PowerAttack(_, _, dmg, cost) => victim.copy(victim.life, rage = victim.rage - cost)
        }

      }
    }


    gameLoop()
  }

}
