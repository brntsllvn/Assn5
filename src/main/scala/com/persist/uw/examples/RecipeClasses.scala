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

  case class Ingredient(
                         val name: String,
                         val unitOfMeasurement: String,
                         val numberOfUnitsInContainer: Int,
                         val pricePerContainer: Int
                       )

  class Recipe(ingredients: List[Ingredient]) {

    def CalculateRecipeCost: Int = {
      def CalculateRecipeCostWithAccum(ingredients: List[Ingredient], accum: Int): Int = {
        ingredients match {
          case head :: tail => CalculateRecipeCostWithAccum(tail, accum + head.pricePerContainer)
          case Nil => accum
        }
      }
      CalculateRecipeCostWithAccum(ingredients, 0)
    }
  }


  //  case class Warm(name: String, unit: String, pack: Int, price: Cost) extends Ingredient

  // Add the additional classes needed here

}
