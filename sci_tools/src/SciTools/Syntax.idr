module SciTools.Syntax

import Text.Lexer
import Generics.Derive

import SciTools.Units

%default total
%language ElabReflection

number = some (digit <|> is '.')
si_prefix = oneOf "YZEPTGMkmunpfazy"
unit_abbrev = choiceMap exact $ the (List String) [
 "s",
 "m",
 "g",
 "l",
 "Pa",
 "N",
 "J"
]

data Token =
    TNumber String
  | TSpaces
  | TSiPrefix String
  | TUnitAbbrev String
%runElab derive "Token" [Generic, Meta, Eq, Ord, Show]

tokenMap : TokenMap Token
tokenMap = [
  (number, TNumber),
  (spaces <|> is '_', const TSpaces),
  (unit_abbrev, TUnitAbbrev),
  (si_prefix, TSiPrefix)
]

export
lexQty : String -> Maybe $ List Token
lexQty s = case lex tokenMap s of
                (ts, (_,_,"")) => Just
                  (filter (\case TSpaces => False; _ => True)
                    (map val ts))
                (_,  t)        => Nothing

export
parseQty : String -> Maybe Qty
parseQty s = do
  tokens <- lexQty s
  (v_s, prefix_s, abbrev) <- the (Maybe (String, String, String)) $ case tokens of
              [ TNumber n, TSiPrefix p, TUnitAbbrev a ] => Just (n, p, a)
              [ TNumber n, TUnitAbbrev a ] => Just (n, "", a)
              _ => Nothing
  v <- parseDouble v_s
  prefix_exp <- exp_of_prefix prefix_s
  (u, unit_exp) <- unit_of_abbrev abbrev
  pure $ MkQty { v = v * (pow 10 $ cast (prefix_exp + unit_exp)), u }

test : IO ()
test = do
  res <- pure $ parseQty "123.45Pa"
  putStrLn $ show res
