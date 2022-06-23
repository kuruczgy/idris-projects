module Server

import Interfaces
import Db

data Request =
    ReqEcho Bits8
  | ReqPrint (List Bits8)

export
implementation Serialize Request where
  serialize (ReqEcho d) = [0, d]
  serialize (ReqPrint l) = 1 :: l

  deserialize [0, d] = Just $ ReqEcho d
  deserialize (1 :: l) = Just $ ReqPrint l
  deserialize _ = Nothing

  serialize_correct (ReqEcho _) = Refl
  serialize_correct (ReqPrint _) = Refl

data Reply =
    ReplyEcho Bits8

export
implementation Serialize Reply where
  serialize (ReplyEcho d) = [0, d]

  deserialize [0, d] = Just $ ReplyEcho d
  deserialize _ = Nothing

  serialize_correct (ReplyEcho _) = Refl

public export
Server : Type -> Type -> Type
Server req res = req -> IO $ Maybe res

export
get_server : RecipeDb -> Server Request Reply
get_server db =
  let
    server : Server Request Reply
    server (ReqEcho d) = pure $ Just $ ReplyEcho d
    server (ReqPrint d) = do
      putStrLn "ReqPrint \{show d}"
      pure Nothing
  in
    server
