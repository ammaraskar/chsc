module Core.Data where

import Utilities


type Arity = Int
type DataCon = String

dataTypes :: [[(DataCon, Arity)]]
dataTypes = [
    [("()"     , 0)],
    [("(,)"    , 2)],
    [("(,,)"   , 3)],
    [("(,,,)"  , 4)],
    [("[]"     , 0),
     ("(:)"    , 2)],
    [("Left"   , 1),
     ("Right"  , 1)],
    [("True"   , 0),
     ("False"  , 0)],
    [("Just"   , 1),
     ("Nothing", 0)],
    [("MkU"    , 1)], -- GHCBug
    [("Z"      , 0),  -- Exp3_8
     ("S"      , 1)], -- Exp3_8
    [("Leaf"   , 1),  -- SumTree
     ("Branch" , 2)], -- SumTree
    [("Empty"  , 0),  -- ZipTreeMaps
     ("Node"   , 3)], -- ZipTreeMaps
    [("Wheel1" , 2)], -- Wheel-Sieve1
    [("Wheel2" , 3)], -- Wheel-Sieve2
    [("A"      , 0),  -- KMP
     ("B"      , 0)], -- KMP
    [("H"      , 0),  -- Paraffins
     ("C"      , 3),  -- Paraffins
     ("BCP"    , 2),  -- Paraffins
     ("CCP"    , 4)],  -- Paraffins
    [("App"    , 2),   -- Expr
     ("Case"   , 2),
     ("Lam"    , 2),
     ("Let"    , 2),
     ("Var"    , 1),
     ("Con"    , 2)],
    [("Pat"    , 2)],
    [("Done"   , 3),
     ("Error"  , 1)],   -- ParserResult
    [("Parser" , 1)]   -- Parser
  ]

dataConFriendlyName :: DataCon -> Maybe String
dataConFriendlyName dc = case dc of
    "()"    -> Just "Tup0"
    "(,)"   -> Just "Tup2"
    "(,,)"  -> Just "Tup3"
    "(,,,)" -> Just "Tup4"
    "[]"    -> Just "Nil"
    "(:)"   -> Just "Cons"
    _       -> Nothing

dataConArity :: DataCon -> Arity
dataConArity have_dc = case [arity | dt <- dataTypes, (dc, arity) <- dt, dc == have_dc] of
  [arity] -> arity
  _       -> panic "dataConArity" (text have_dc)

dataConSiblings :: DataCon -> [(DataCon, Arity)]
dataConSiblings have_dc = case [dt | dt <- dataTypes, Just _ <- [lookup have_dc dt]] of
  [dt] -> dt
  _    -> panic "dataConSiblings" (text have_dc)
