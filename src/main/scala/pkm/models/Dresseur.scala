package pkm.models


case class Dresseur(life: Int, rage: Int, pokemons: Seq[Pokemon], ia: IA) {
  def action(): Action = {
    ia.choiceAction(this)
  }

  def firstAlivePokemon(): Option[Pokemon] = {
    pokemons.find(p => p.stats.hp > 0)
  }


}
