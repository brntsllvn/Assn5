package LambdaTest

import com.fortysevendeg.lambdatest._
import LambdaTest.RecipeExample._
import com.persist.uw.examples.RecipeClasses._

// Replace each nyi and nop with needed code

class TestRecipes extends LambdaTest {

  def nyi: LambdaAct = assert(false, "not yet implemented")

  def nop: Unit = ()

  import RecipeExample._

  def act = {
    val rawTomato = RawIngredient("tomato", "tomato", 1, 200)
    val rawBacon = RawIngredient("bacon", "strip", 30, 400)
    val rawBread = RawIngredient("bread", "slice", 20, 200)
    val rawLettuce = RawIngredient("lettuce", "leaf", 30, 200)
    val rawSwissCheese = RawIngredient("swiss cheese", "slice", 20, 200)
    val rawEggs = RawIngredient("eggs", "egg", 12, 300)
    val rawCream = RawIngredient("cream", "T", 10, 200)
    val rawSalt = RawIngredient("salt", "pinch", 100, 100)
    val rawPepper = RawIngredient("pepper", "pinch", 100, 1)
    val rawButter = RawIngredient("butter", "T", 12, 200)

    val bltTomato = RecipeIngredient("blt", rawTomato, 1)
    val bltBacon = RecipeIngredient("blt", rawBacon, 3)
    val bltBread = RecipeIngredient("blt", rawBread, 2)
    val bltLettuce = RecipeIngredient("lettuce", rawLettuce, 2)
    val bltIngredients = List(bltTomato, bltBacon, bltBread, bltLettuce)

    val seEggs = RecipeIngredient("scrambled eggs", rawEggs, 3)
    val seSalt = RecipeIngredient("scrambled eggs", rawSalt, 1)
    val sePepper = RecipeIngredient("scrambled eggs", rawPepper, 1)
    val seButter = RecipeIngredient("scrambled eggs", rawButter, 1)
    val scrambledEggsIngredients = List(seButter, sePepper, seSalt, seEggs)
    val scrambledEggs = Recipe(scrambledEggsIngredients)

    val quicheCream = RecipeIngredient("quiche", rawCream, 10)
    val quicheEgg = RecipeIngredient("quiche", rawEggs, 3)
    val quicheSwissCheese = RecipeIngredient("quiche", rawSwissCheese, 2)
    val quicheBacon = RecipeIngredient("quiche", rawBacon, 3)
    val quicheIngredients = List(quicheCream, quicheSwissCheese, quicheBacon, quicheEgg)
    val quiche = Recipe(quicheIngredients)

    test("how does flatten work?"){
      val flatStuffs = List(
        List("hi stuff", "bye"),
        List("bye", "eh?")
      ).flatten

      assertEq(flatStuffs,List("hi stuff","bye","bye","eh?"))
    } +
    test("how about flatten with complex stuffs"){
      val flatRawIngredients = List(
        List(rawBacon, rawBread),
        List(rawButter, rawCream),
        List(rawSalt, rawPepper)
      ).flatten

      assertEq(flatRawIngredients, List(rawBacon, rawBread, rawButter, rawCream, rawSalt, rawPepper))
    } +
    test("does nested list equality work?") {
      assertEq(List(List(1,2)),List(List(1,2)))
    } +
    test("What is the cost of the ingredients in a blt?") {
      val expectedBltCost = (bltTomato.rawIngredient.pricePerContainer
        + bltBacon.rawIngredient.pricePerContainer
        + bltBread.rawIngredient.pricePerContainer
        + bltLettuce.rawIngredient.pricePerContainer)
      val formattedExpectedBltCost = Cost(expectedBltCost).toString

      val blt = Recipe(bltIngredients)
      val actualBltCost = blt.CalculateRecipeCost
      val formattedActualBltCost = Cost(actualBltCost).toString

      assertEq(formattedActualBltCost, formattedExpectedBltCost, "blt costs")
      // assertEq(cost,cost,"blt costs")
    } +
      test("What ingredients do I need to make a quiche?") {
        val formattedQuicheIngredients = quiche.FormatRecipeIngredients

        val expectedQuicheIngredients = List("3 egg eggs", "3 strip bacon", "2 slice swiss cheese", "10 T cream")

        assertEq(formattedQuicheIngredients, expectedQuicheIngredients, "quiche ingredients")
      } +
      test("What ingredients do I need to make scrambled eggs?") {
        val expectedScrambledEggsIngredients = List("3 egg eggs", "1 pinch salt", "1 pinch pepper", "1 T butter")
        val formattedScrambledEggsIngredients = scrambledEggs.FormatRecipeIngredients

        assertEq(formattedScrambledEggsIngredients, expectedScrambledEggsIngredients, "scrambled eggs ingredients")
      } +
      test("What ingredients do I have in my house?") {
        val homePantry = HomePantry(List())
        val actualIngredientsInHome = homePantry.GetInventory

        val expectedIngredientsInHome = List()

        assertEq(actualIngredientsInHome, expectedIngredientsInHome, "in my house")
      } +
      test("Buying everything I need to make scrambled eggs") {
        val homePantry = HomePantry(List())
        val homePantryAfterShopping = homePantry.GoShoppingFor(List(scrambledEggs))

        val expectedShoppingList = scrambledEggs.FormatRawIngredients

        assertEq(homePantryAfterShopping, expectedShoppingList, "scrambled eggs now in inventory")
      } +
      test("Buying everything I need to make scrambled eggs and a quiche") {
        val homePantry = HomePantry(List())
        val homePantryAfterShopping = homePantry.GoShoppingFor(List(scrambledEggs, quiche))

        val se = scrambledEggs.FormatRawIngredients
        val qu = quiche.FormatRawIngredients
        val expectedShoppingList = List(se,qu)

        assertEq(homePantryAfterShopping, expectedShoppingList, "scrambled eggs and quiche now in inventory")
        // this is in addition to what is already in house
        // Note: you can only buy whole packages (so round up)
        // Report the cost of groceries purchased

      }
    // +
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
}


object TestRecipes {
  def main(args: Array[String]): Unit = {
    run("recipes", new TestRecipes)
  }
}
