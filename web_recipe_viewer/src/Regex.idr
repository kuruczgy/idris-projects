module Regex

%foreign "javascript:lambda:x=>x==null?0:1"
prim__jsIsNull : AnyPtr -> Int

jsCast : AnyPtr -> a
jsCast = believe_me

jsMaybe : AnyPtr -> Maybe AnyPtr
jsMaybe x = if prim__jsIsNull x /= 0 then Just x else Nothing

%foreign "javascript:lambda:s=>new RegExp(s)"
prim__jsRegExp : String -> AnyPtr

%foreign "javascript:lambda:(r,s)=>s.match(r)"
prim__jsRegExpMatch : AnyPtr -> String -> AnyPtr

%foreign "javascript:lambda:(m,i)=>m[i]"
prim__jsMatchGroup : AnyPtr -> Int -> AnyPtr

export
data RegExp = MkRegExp AnyPtr

export
create : String -> RegExp
create = MkRegExp . prim__jsRegExp

data RegExpMatch = MkRegExpMatch AnyPtr

export
match : RegExp -> String -> Maybe RegExpMatch
match (MkRegExp r) s = map MkRegExpMatch $ jsMaybe $ prim__jsRegExpMatch r s

export
group : RegExpMatch -> Int -> Maybe String
group (MkRegExpMatch m) i = map jsCast $ jsMaybe $ prim__jsMatchGroup m i
