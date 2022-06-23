module Interfaces

public export
interface Serialize a where
  serialize : a -> List Bits8
  deserialize : List Bits8 -> Maybe a
  0 serialize_correct : (elem : a) -> deserialize (serialize elem) = Just elem
