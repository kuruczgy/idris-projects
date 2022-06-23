module Db

import Glue

public export
interface RecipeDb where
  constructor MkRecipeDb
  add_recipe : String -> String -> IO $ Maybe ()
  update_recipe : String -> String -> IO $ Maybe ()
  get_recipe : String -> IO $ Maybe String

export
init_recipe_db : DbConnection -> IO $ Maybe RecipeDb
init_recipe_db db = do
  Just [] <- db.exec
    """
      create table if not exists recipes
		  (
        id serial primary key,
        title text not null unique,
        body text not null
      )
    """
    []
    | _ => pure Nothing
  let
    add_recipe : String -> String -> IO $ Maybe ()
    add_recipe title body = do
      Just [] <- db.exec
        """
        insert into recipes (title, body) values ($1, $2)
        """
        [title, body]
        | _ => pure Nothing
      pure $ Just ()
    update_recipe : String -> String -> IO $ Maybe ()
    update_recipe title body = do
      Just [] <- db.exec
        """
        update recipes
        set body = $2
        where title = $1
        """
        [title, body]
        | _ => pure Nothing
      pure $ Just ()
    get_recipe : String -> IO $ Maybe String
    get_recipe title = do
      Just [row] <- db.exec
        """
        select body from recipes
        where title = $1
        """
        [title]
        | _ => pure Nothing
      pure $ Just row
  pure $ Just $ MkRecipeDb { add_recipe, update_recipe, get_recipe }
