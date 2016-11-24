package com.persist.uw.examples

object RecipeClasses {

  //  def DollarsCents(dollars: Int = 0, cents: Int = 0) = Cost
  //

  case class Cost(cents: Int) {
    def *(i: Int) = Cost(i * cents)

    def /(i: Int) = Cost((cents + i - 1) / i)

    private def dollarSign = "$"

    override def toString: String = f"$dollarSign${cents / 100}.${cents % 100}%02d"

  }

  case class HomeInventoryIngredient(rawIngredient: RawIngredient, inventory: Int) {}

  case class HomePantry(inventory: List[HomeInventoryIngredient]) {

    def GetInventory: List[String] = {
      def GetInventoryWithAccum(inventory: List[HomeInventoryIngredient], accum: List[String]): List[String] = {
        inventory match {
          case firstItemInInventory :: remainingItemsInInventory => {
            GetInventoryWithAccum(remainingItemsInInventory,
              (firstItemInInventory.inventory.toString
                + " "
                + firstItemInInventory.rawIngredient.unitOfMeasurement.toString
                + " "
                + firstItemInInventory.rawIngredient.name.toString)
                :: accum)
          }
          case Nil => accum
        }
      }
      GetInventoryWithAccum(inventory, Nil)
    }

    def GoShoppingFor(recipes: List[Recipe]): List[String] = {
      def GoShoppingWithAccum(recipes: List[Recipe], accum: List[List[String]]): List[List[String]] = {
        recipes match {
          case firstRecipe :: remainingRecipes => {
            GoShoppingWithAccum(remainingRecipes, firstRecipe.FormatRawIngredients :: accum)
          }
          case Nil => accum
        }
      }
//      def SpecialFlatten(ls: List[Any]): List[Any] = ls flatMap {
//        case i: List[_] => SpecialFlatten(i)
//        case e => List(e)
//      }
//      SpecialFlatten(GoShoppingWithAccum(recipes, Nil))
      GoShoppingWithAccum(recipes, Nil).flatten
    }

  }


  case class RawIngredient(
                            val name: String,
                            val unitOfMeasurement: String,
                            val numberOfUnitsInContainer: Int,
                            val pricePerContainer: Int
                          ) {
    def format: String = {
      (this.numberOfUnitsInContainer +
        " " +
        this.unitOfMeasurement +
        " " +
        this.name)
    }
  }

  case class RecipeIngredient(
                               val recipe: String,
                               val rawIngredient: RawIngredient,
                               val numberOfUnits: Int
                             ) {
    def format: String = {
      (this.numberOfUnits.toString +
        " " +
        this.rawIngredient.unitOfMeasurement.toString +
        " " +
        this.rawIngredient.name.toString)
    }
  }

  case class Recipe(ingredients: List[RecipeIngredient]) {

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

    def FormatRecipeIngredients: List[String] = {
      def FormatIngredientsWithAccum(ingredients: List[RecipeIngredient], accum: List[String]): List[String] = {
        ingredients match {
          case firstIngredient :: remainingIngredients => {
            FormatIngredientsWithAccum(remainingIngredients, firstIngredient.format :: accum)
          }
          case Nil => accum
        }
      }
      FormatIngredientsWithAccum(ingredients, Nil)
    }

    def FormatRawIngredients: List[String] = {
      def FormatRawIngredientsWithAccum(ingredients: List[RawIngredient], accum: List[String]): List[String] = {
        ingredients match {
          case firstIngredient :: remainingIngredients => {
            FormatRawIngredientsWithAccum(remainingIngredients, firstIngredient.format :: accum)
          }
          case Nil => accum
        }
      }
      FormatRawIngredientsWithAccum(ingredients.map(_.rawIngredient), Nil)
    }
  }


  //  case class Warm(name: String, unit: String, pack: Int, price: Cost) extends Ingredient

  // Add the additional classes needed here

}
