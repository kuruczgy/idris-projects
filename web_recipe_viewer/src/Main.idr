module Main

import Data.IORef
import Data.Maybe
import Data.String
import Data.String.Extra
import Data.SortedMap

import SciTools.RecipeSyntax
import SciTools.Units
import SciTools.Syntax

%foreign """
javascript:lambda:(
  onValueChange,
) => {
  window.onload = () => {
    const metaTag = document.createElement('meta')
    metaTag.name = 'viewport'
    metaTag.content = 'width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0'
    document.head.append(metaTag)

    // Add style
    const style = document.createElement('style')
    style.textContent = `
      #root {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 20px;
      }
      #recipe {
        width: max(30vw, 300px);
        height: max(30vh, 200px);
      }
    `
    document.head.append(style)

    // Add layout
    const root = document.createElement('div')
    root.id = 'root'

    const options = [ 'oven_baked_chicken_breast', 'waffles' ]
    const select = document.createElement('select')
    select.id = 'recipeSelect'
    for (const optionId of options) {
      const option = document.createElement('option')
      option.value = option.text = optionId
      select.append(option)
    }
    root.append(select)

    const recipe = document.createElement('textarea')
    recipe.id = 'recipe'
    root.append(recipe)

    const ratio = document.createElement('input')
    ratio.id = 'ratio'
    ratio.type = 'number'
    ratio.value = '1'
    ratio.step = '0.1'
    root.append(ratio)

    const display = document.createElement('code')
    display.id = 'display'
    root.append(display)

    document.body.append(root)

    for (const inputSelector of [ '#recipe', '#ratio', '#recipeSelect' ]) {
      document.querySelector(inputSelector).oninput = (e) =>
        onValueChange(inputSelector)(e.target.value)()
    }

    onValueChange('#recipeSelect')(select.value)()
  }
}
"""
prim__initApp :
  (onValueChange : String -> String -> PrimIO ()) ->
  PrimIO ()

%foreign "javascript:lambda:(s,v)=>document.querySelector(s).value=v"
prim__updateValue : String -> String -> PrimIO ()

%foreign "javascript:lambda:(s)=>document.querySelector('#display').innerText=s"
prim__updateDisplay : String -> PrimIO ()

record State where
  constructor MkState
  ratio : Double
  recipe : String

stateUpdate : IORef State -> (State -> State) -> IO ()
stateUpdate ref f = do
  modifyIORef ref f
  state <- readIORef ref
  display <- pure (let
    recipes := parseRecipes state.recipe
    scaledRecipes := map ((*) state.ratio) recipes
    in
    join "\n" $ map show scaledRecipes)
  primIO $ prim__updateDisplay display

storedRecipes : SortedMap String String
storedRecipes = fromList [
  (
    "oven_baked_chicken_breast",
    """
    ## Oven Baked Chicken Breast
    - 2kg chicken breasts
    - 30ml olive oil
    - 6g smoked paprika
    - 8g garlic powder
    - 4g oregano
    - 12g table salt
    - 3g ground black pepper
    """
  ),
  (
    "waffles",
    """
    ## Almond flour waffles
    - 3Pa eggs
    - 14g vanillin erythritol
    - 20g erythritol
    - 130g milk (or plant alternative)
    - 230g almond flour
    - 10g baking soda
    - 70g butter
    """
  )
]

onValueChange : IORef State -> String -> String -> PrimIO ()
onValueChange ref selector value = toPrim $
  case selector of
    "#recipe" => stateUpdate ref { recipe := value }
    "#ratio" => stateUpdate ref { ratio := fromMaybe 0 $ parseDouble value }
    "#recipeSelect" =>
      let recipe := fromMaybe "" $ lookup value storedRecipes in do
        primIO $ prim__updateValue "#recipe" recipe
        stateUpdate ref { recipe := recipe }
    _ => pure ()

main : IO ()
main = do
  ref <- newIORef $ MkState 1 ""
  primIO $ prim__initApp
    (onValueChange ref)
