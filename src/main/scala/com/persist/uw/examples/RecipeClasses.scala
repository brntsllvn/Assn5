package com.persist.uw.examples

object RecipeClasses {

  //  def DollarsCents(dollars: Int = 0, cents: Int = 0) = Cost
  //
  //  case class Cost(cents: Int) {
  //    def *(i: Int) = Cost(i * cents)
  //    def /(i: Int) = Cost((cents + i - 1) / i)
  //    private def dollarSign = "$"
  //      override def toString: String = f"$dollarSign${cents / 100}.${cents % 100}%02d"
  //
  //  }

  case class RawIngredient(
                            val name: String,
                            val unitOfMeasurement: String,
                            val numberOfUnitsInContainer: Int,
                            val pricePerContainer: Int
                          )

  case class RecipeIngredient(
                              val recipe: String,
                              val rawIngredient: RawIngredient,
                              val numberOfUnits: Int
                             )

  class Recipe(ingredients: List[RecipeIngredient]) {

    def CalculateRecipeCost: Int = {
      def CalculateRecipeCostWithAccum(ingredients: List[RecipeIngredient], accum: Int): Int = {
        ingredients match {
          case firstIngredient :: remainingIngredients => {
            CalculateRecipeCostWithAccum(remainingIngredients, accum + firstIngredient.rawIngredient.pricePerContainer)
          }
          case Nil => accum
        }
      }
      CalculateRecipeCostWithAccum(ingredients, 0)
    }
  }


  //  case class Warm(name: String, unit: String, pack: Int, price: Cost) extends Ingredient

  // Add the additional classes needed here

}
