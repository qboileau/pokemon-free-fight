package pkm.models


case class Trainer(name: String, life: Int, rage: Int, pokemons: Seq[Pokemon], ia: IA) {
  def action(arena: Arena): Action = {
    ia.choiceAction(this, arena)
  }

  def firstAlivePokemon(): Option[Pokemon] = {
    pokemons.find(p => p.stats.hp > 0)
  }

  def isDead() = life <= 0

}
