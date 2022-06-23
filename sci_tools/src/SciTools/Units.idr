module SciTools.Units

import Data.SortedMap
import Data.String
import Data.Maybe
import Debug.Trace
import Generics.Derive

%default total
%language ElabReflection

public export
record DerivedUnit where
  constructor MkDerivedUnit
  s: Int8
  m: Int8
  kg: Int8
%runElab derive "DerivedUnit" [Generic, Meta, Eq, Ord, Show]
%name DerivedUnit du

map : (Int8 -> Int8) -> DerivedUnit -> DerivedUnit
map f du = MkDerivedUnit { s = f du.s, m = f du.m, kg = f du.kg }

combine : (Int8 -> Int8 -> Int8) -> DerivedUnit -> DerivedUnit -> DerivedUnit
combine f a b = MkDerivedUnit { s = f a.s b.s, m = f a.m b.m, kg = f a.kg b.kg }

public export
du_to_string : DerivedUnit -> String
du_to_string du =
  let comp : String -> Int8 -> String =
    \k, v => case v of
                  0 => ""
                  1 => k
                  _ => k ++ "^" ++ prim__cast_Int8String v in
  comp "s" du.s ++ comp "m" du.m ++ comp "kg" du.kg

namespace Units
  public export
  kg : DerivedUnit
  kg = MkDerivedUnit { s = 0, m = 0, kg = 1 }

  export
  m3 : DerivedUnit
  m3 = MkDerivedUnit { s = 0, m = 3, kg = 0 }

  export
  J : DerivedUnit
  J = MkDerivedUnit { s = -2, m = 2, kg = 1 }

  public export
  empty : DerivedUnit
  empty = MkDerivedUnit { s = 0, m = 0, kg = 0 }

prefix_list : List (String, Int8)
prefix_list = [
  ("Y", 24),
  ("Z", 21),
  ("E", 18),
  ("P", 15),
  ("T", 12),
  ("G", 9),
  ("M", 6),
  ("k", 3),
  ("", 0),
  ("m", -3),
  ("u", -6),
  ("n", -9),
  ("p", -12),
  ("f", -15),
  ("a", -18),
  ("z", -21),
  ("y", -24)
]

prefix_of_exp : Int8 -> Maybe String
prefix_of_exp =
  let m = fromList $ map (\(p, e) => (e, p)) prefix_list in
  \e => Data.SortedMap.lookup e m

export
exp_of_prefix : String -> Maybe Int8
exp_of_prefix =
  let m = fromList prefix_list in
  \p => Data.SortedMap.lookup p m

unit_abbrevs_list : List (String, DerivedUnit, Int8)
unit_abbrevs_list = let e := Units.empty in [
  ("s", { s := 1 } e, 0),
  ("m", { m := 1 } e, 0),
  ("g", { kg := 1 } e, -3),
  ("l", { m := 3 } e, -3),
  ("Pa", { s := -2, m := -1, kg := 1 } e, 0),
  ("N", { s := -2, m := 1, kg := 1 } e, 0),
  ("J", { s := -2, m := 2, kg := 1 } e, 0)
]

export
unit_of_abbrev : String -> Maybe (DerivedUnit, Int8)
unit_of_abbrev =
  let m = fromList unit_abbrevs_list in
  \s => Data.SortedMap.lookup s m

abbrev_of_unit : DerivedUnit -> Maybe (String, Int8)
abbrev_of_unit =
  let m = fromList $ map (\(a, du, e) => (du, a, e)) unit_abbrevs_list in
  \du => Data.SortedMap.lookup du m

public export
record Qty where
  constructor MkQty
  v: Double
  u: DerivedUnit
%runElab derive "Qty" [Generic, Meta, Eq, Ord, Show]
%name Qty q

op_additive : (Double -> Double -> Double) -> Qty -> Qty -> Maybe Qty
op_additive f a b = if a.u == b.u then
                      Just $ MkQty { v = f a.v b.v, u = a.u }
                      else Nothing
export
(+) : Qty -> Qty -> Maybe Qty
(+) = op_additive (+)

export
(-) : Qty -> Qty -> Maybe Qty
(-) = op_additive (-)

op_multiplicative : (Double -> Double -> Double) -> (Int8 -> Int8 -> Int8) -> Qty -> Qty -> Qty
op_multiplicative f f' a b = MkQty { v = f a.v b.v, u = combine f' a.u b.u }

export
(*) : Qty -> Qty -> Qty
(*) = op_multiplicative (*) (+)

export
(/) : Qty -> Qty -> Qty
(/) = op_multiplicative (/) (-)

namespace ScalarOps
  export
  (*) : Double -> Qty -> Qty
  (*) x (MkQty { v, u }) = MkQty { v = x * v, u }

grab : Qty -> DerivedUnit -> Maybe Double
grab q u = if q.u == u then Just q.v else Nothing

-- re_qty : Regex.RegExp
-- re_qty = Regex.create #"^([\d\.]+)\s*([YZEPTGMkmunpfazy]|)_?(\w+)$"#
-- 
-- export
-- qty_of_string : String -> Maybe Qty
-- qty_of_string s = do
--   m <- Regex.match re_qty s
-- 
--   v_s <- Regex.group m 1
--   prefix_s <- Regex.group m 2
--   abbrev <- Regex.group m 3
-- 
--   v <- parseDouble v_s
--   prefix_exp <- exp_of_prefix prefix_s
--   (u, unit_exp) <- unit_of_abbrev abbrev
--   Just $ MkQty { v = v * (pow 10 $ cast (prefix_exp + unit_exp)), u }

export
implementation Show Qty where
  show (MkQty v u) =
    let log10 = \x => log x / log 10 in
    let clamp = \min, max, x => the Double
                  (if x < min then min else
                    if max < x then max else x) in
    let si_exp = \v, ex : Int8 =>
                 let e = clamp (-24) 24 ((log10 $ abs v) + (cast {to=Double} ex)) / 3.0 in
                    3 * (cast { to = Int } $ floor e) in
    let (name, unit_ex) = fromMaybe (du_to_string u, 0) $ abbrev_of_unit u in
    let si_ex = si_exp v (-unit_ex) in
    let v = v / (pow 10 $ cast (si_ex + cast unit_ex)) in
    cast (roundDouble v) ++ fromMaybe "" (prefix_of_exp $ cast si_ex) ++ name
  where
    roundDouble : Double -> Double
    roundDouble d = floor (d * 1000 + 0.5) / 1000
