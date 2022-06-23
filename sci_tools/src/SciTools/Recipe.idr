module SciTools.Recipe

import Data.Vect
import Data.String.Extra

import SciTools.Units

%default total

public export
record Prop where
  constructor MkProp
  name : String
  unit : DerivedUnit

implementation Show Prop where
  show p = p.name ++ " : " ++ (du_to_string p.unit)

public export
data PropList : (0 _ : Vect n Prop) -> Type where
  MkPropList : { 0 p : Vect n Prop } -> (Vect n Double) -> PropList p

show : (p : Vect n Prop) -> PropList p -> String
show p (MkPropList l) =
    let rows = map (\(p, v) => prim__cast_DoubleString v ++ " (" ++ show p ++ ")") $ zip p l in
    join "\n" rows

(*) : Double -> PropList p -> PropList p
(*) x (MkPropList l) = MkPropList $ map ((*) x) l

export
(+) : PropList p -> PropList p -> PropList p
(+) (MkPropList a) (MkPropList b) = MkPropList $ map (\(a, b) => a + b) $ zip a b

public export
data Material : (0 _ : Vect n Prop) -> Type where
  Base : { 0 p : Vect n Prop } -> PropList p -> Material p
  Mix : { 0 p : Vect n Prop } -> (Vect (S k) (Double, Material p)) -> Material p

export
createMix : List (Double, Material p) -> Maybe (Material p)
createMix [] = Nothing
createMix (h :: t) = Just $ Mix $ h :: (fromList t)

export
basic_props : Vect ? Prop
basic_props = [
  MkProp { name = "mass", unit = Units.kg },
  MkProp { name = "price", unit = Units.empty }
]

covering
props_of_material : Material p -> PropList p
props_of_material (Base ps) = ps
props_of_material (Mix l) =
  foldl1 (+) $ map (\(r, m) => r * props_of_material m) l

test_mat_1 : Material Recipe.basic_props
test_mat_1 = Mix [
  (1, Base $ MkPropList [1, 100]),
  (2, Base $ MkPropList [1, 100])
]

test1 : props_of_material Recipe.test_mat_1 = MkPropList [3, 300]
test1 = Refl

test3 : prim__cast_DoubleString 1 = "1.0"
test3 = Refl

test4 : join "\n" (the (Vect ? ?) ["a", "b"]) = "a\nb"
test4 = Refl

test2 : show Recipe.basic_props (props_of_material Recipe.test_mat_1) = "3.0 (mass : kg)\n300.0 (price : )"
test2 = Refl
