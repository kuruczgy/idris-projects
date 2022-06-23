module Main

import Data.IORef

import Glue
import Db
import Server
import Interfaces

%default total

listen : Serialize req => Serialize res => Server req res -> IO ()
listen server = do
  clientCounter <- newIORef (the Bits32 0)
  let
    established : Connection -> IO ConnectionHandlers
    established conn = do
      clientId <- readIORef clientCounter
      modifyIORef clientCounter ((+) 1)
      writeQueue <- newIORef (the (List $ List Bits8) [])
      let
        receive : List Bits8 -> IO ()
        receive buf = do
          (Just request) <- pure $ deserialize buf
            | Nothing => pure ()
          (Just response) <- server request
            | Nothing => pure ()
          modifyIORef writeQueue ((::) $ serialize response)
          conn.callback_on_writable
        writable : IO $ Maybe $ List Bits8
        writable = do
          h :: t <- readIORef writeQueue
            | [] => pure Nothing
          writeIORef writeQueue t
          conn.callback_on_writable
          pure $ Just h
        closed : IO ()
        closed = do
          putStrLn "[idris] closed \{show clientId}"
      putStrLn "[idris] established \{show clientId}"
      pure $ MkConnectionHandlers { receive, writable, closed }
  glue.listen established

main : IO ()
main = do
  Just db_conn <- glue.db_connect "postgresql://postgres:asdfg@localhost/postgres"
    | _ => putStrLn "db_init failed"
  Just db <- init_recipe_db db_conn
    | _ => pure ()
  listen $ get_server db
