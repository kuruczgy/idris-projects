module SciTools.RecipeSyntax

import Data.String.Extra
import Data.Vect
import Generics.Derive

import SciTools.Units
import SciTools.Recipe
import SciTools.Syntax

%default total
%language ElabReflection

public export
record Recipe where
  constructor MkRecipe
  name : String
  items : List (Qty, String)
%runElab derive "Recipe" [Generic, Meta, Eq, Ord]

export
implementation Show Recipe where
  show (MkRecipe { name, items }) =
    let rows = map (\(q, name) => "- \{show q} \{name}\n") items in
    "## \{name}\n" ++ join "" rows

-- props : Vect ? Prop
-- props = [ MkProp { name = "mass", unit = Units.kg } ]
-- PropList = Recipe.PropList props
-- covering
-- propListOfRecipe : String -> (List Recipe) -> PropList
-- propListOfRecipe inputName recipes =
--   case find (\(MkRecipe { name, items }) => name == inputName) recipes of
--        Just recipe =>
--           let MkPropList [mass] =
--             foldl Recipe.(+) (MkPropList [0]) $
--               map (\(q, name) => propListOfRecipe name recipes) recipe.items in
--           MkPropList [1]
--        Nothing => MkPropList [1]

export
(*) : Double -> Recipe -> Recipe
(*) x (MkRecipe { name, items }) =
  MkRecipe { name, items = map (\(q, name) => (x * q, name)) items }

export
parseRecipes : String -> List Recipe
parseRecipes s = linesToMats $ map parseLine (lines s)
where
  data Line =
      Section String
    | ListItem (Qty, String)

  breakOnFirstSpace : String -> (String, String)
  breakOnFirstSpace s =
    let (head, tail) := break (== ' ') s in
    (head, drop 1 tail)

  parseListItem : String -> Maybe (Qty, String)
  parseListItem rawListItem = do
    (rawQty, tail) <- pure $ breakOnFirstSpace rawListItem
    qty <- parseQty rawQty
    pure $ (qty, tail)

  parseLine : String -> Maybe Line
  parseLine rawLine =
    let (key, tail) := breakOnFirstSpace rawLine in
    case key of
         "##" => Just $ Section tail
         "-" => map ListItem (parseListItem tail)
         _ => Nothing

  Acc : Type
  Acc = (Maybe Recipe, List Recipe)

  finishRecipe : Maybe Recipe -> List Recipe -> List Recipe
  finishRecipe Nothing list = list
  finishRecipe (Just $ MkRecipe { name, items }) list =
    (MkRecipe { name, items = reverse items }) :: list

  addLineToRecipes : Acc -> (Maybe Line) -> Acc
  addLineToRecipes (current, recs) line =
    case line of
         Nothing => (current, recs)
         Just (Section name) => (Just $ MkRecipe { name, items = [] }, finishRecipe current recs)
         Just (ListItem listItem) =>
           case current of
                Nothing => (current, recs)
                Just (MkRecipe { name, items }) =>
                  (Just $ MkRecipe { name, items = listItem :: items }, recs)

  linesToMats : List (Maybe Line) -> List Recipe
  linesToMats lines =
    let (last, recs) = foldl addLineToRecipes (Nothing, []) lines in
    reverse $ finishRecipe last recs

