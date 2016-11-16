package LambdaTest

import com.fortysevendeg.lambdatest._
import LambdaTest.RecipeExample._
import com.persist.uw.examples.RecipeClasses._

// Replace each nyi and nop with needed code

class TestRecipes extends LambdaTest {

  def nyi: LambdaAct = assert(false, "not yet implemented")

  def nop: Unit = ()

  val r = RecipeExample

  import r._

  def act =
    test("What is the cost of the ingredients in a blt?") {
      val rawTomato = RawIngredient("tomato", "tomato", 1, 2)
      val bltTomato = RecipeIngredient("blt", rawTomato, 1)

      val rawBacon = RawIngredient("bacon", "slice", 30, 4)
      val bltBacon = RecipeIngredient("blt", rawBacon, 3)

      val rawBread = RawIngredient("bread", "slice", 20, 2)
      val bltBread = RecipeIngredient("blt", rawBread, 2)

      val rawLettuce = RawIngredient("lettuce", "leaf", 30, 2)
      val bltLettuce = RecipeIngredient("lettuce", rawLettuce, 2)

      val bltIngredients = List(bltTomato, bltBacon, bltBread, bltLettuce)

      val expectedBltCost = (bltTomato.rawIngredient.pricePerContainer
        + bltBacon.rawIngredient.pricePerContainer
        + bltBread.rawIngredient.pricePerContainer
        + bltLettuce.rawIngredient.pricePerContainer)

      val blt = new Recipe(bltIngredients)
      val actualBltCost = blt.CalculateRecipeCost

      assertEq(actualBltCost, expectedBltCost, "blt costs")


      // assertEq(cost,cost,"blt costs")
      //      nyi
    }

//      test("What ingredients do I need to make a quiche?") {
//        nyi
//      } +
//      test("What ingredients do I have in my house?") {
//        nyi
//      } +
//      test("Buying everything I need to make scrambled eggs and a quiche") {
//        // this is in addition to what is already in house
//        // Note: you can only buy whole packages (so round up)
//        // Report the cost of groceries purchased
//        nyi
//      } +
//      test("What ingredients do I have in my house?") {
//        nyi
//      } +
//      label("Preparing a quiche") {
//        exec {
//          // this should remove the ingredients used from the inventory
//          nop
//        }
//      } +
//      test("What ingredients do I have in my house?") {
//        // After the quiche wa prepared
//        nyi
//      } +
//      test("What recipes in my cookbook use bacon?") {
//        nyi
//      } +
//      test("How do I prepare a blt?") {
//        nyi
//      } +
//      test("What recipes in my cookbook do I now have enough ingredients to prepare?") {
//        nyi
//      } +
//      test("Going shopping") {
//        // Set up shopping list with 10 tomatoes and 50 T butter
//        // Only whole packages can be purchased
//        // Add my purchases to my inventory
//        // Report the cost of the purchase
//        nyi
//      } +
//      test("What ingredients do I have in my house?") {
//        // After shopping
//        nyi
//      }
}


object TestRecipes {
  def main(args: Array[String]): Unit = {
    run("recipes", new TestRecipes)
  }
}
