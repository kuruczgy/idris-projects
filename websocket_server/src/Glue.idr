module Glue

public export
record Connection where
  callback_on_writable : IO ()
  dummy : ()

public export
record DbConnection where
  exec : String -> List String -> IO (Maybe $ List String)
  dummy : ()

public export
record ConnectionHandlers where
  constructor MkConnectionHandlers
  receive : List Bits8 -> IO ()
  writable : IO $ Maybe $ List Bits8
  closed : IO ()

public export
record Glue where
  listen : (established : Connection -> IO ConnectionHandlers) -> IO ()
  db_connect : (connection_string : String) -> IO (Maybe DbConnection)

export
%foreign "C:get_glue,,glue.c"
glue : Glue
