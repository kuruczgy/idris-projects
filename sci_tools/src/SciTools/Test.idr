module SciTools.Test

import RecipeSyntax

import Units
import Data.String.Extra

%default total

assertEqual : Show t => Eq t => String -> t -> t -> IO ()
assertEqual msg a b =
  if a == b then
    pure ()
  else
    putStrLn "mismatch: \{msg}: \{show a} != \{show b}"

recipe1 = """
## Pasta
- 500g almond flour
- 100g water
- 7g baking powder

## Sauce
- 400g tomato sauce
- 2g salt
- 2g basil
- 3g garlic powder

## Pizza
- 100g Sauce
- 200g Pasta
"""

main : IO ()
main = do
  assertEqual
    "recipe1"
    (parseRecipes recipe1)
    [
      MkRecipe { name = "Pasta", items = [
        (MkQty 0.5 Units.kg, "almond flour"),
        (MkQty 0.1 Units.kg, "water"),
        (MkQty 0.007 Units.kg, "baking powder")
      ] },
      MkRecipe { name = "Sauce", items = [
        (MkQty 0.4 Units.kg, "tomato sauce"),
        (MkQty 0.002 Units.kg, "salt"),
        (MkQty 0.002 Units.kg, "basil"),
        (MkQty 0.003 Units.kg, "garlic powder")
      ] },
      MkRecipe { name = "Pizza", items = [
        (MkQty 0.1 Units.kg, "Sauce"),
        (MkQty 0.2 Units.kg, "Pasta")
      ] }
    ]
  assertEqual
    "scale recipe1"
    (join "\n" $ map show $ map ((*) 0.5) $ parseRecipes recipe1)
    """
    ## Pasta
    - 250.0g almond flour
    - 50.0g water
    - 3.5g baking powder

    ## Sauce
    - 200.0g tomato sauce
    - 1.0g salt
    - 1.0g basil
    - 1.5g garlic powder

    ## Pizza
    - 50.0g Sauce
    - 100.0g Pasta

    """
