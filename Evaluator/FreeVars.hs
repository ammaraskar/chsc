{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
module Evaluator.FreeVars (
    WhyLive(..), Liveness, livenessAllFreeVars,
    mkConcreteLiveness, mkPhantomLiveness, emptyLiveness, plusLiveness, plusLivenesses,
    whyLive,

    inFreeVars,
    heapBindingLiveness,
    pureHeapBoundVars, stackBoundVars, stackFrameBoundVars,
    stateFreeVars, stateStaticBinders, stateStaticBindersAndFreeVars
  ) where

import Core.Syntax
import Evaluator.Syntax

import Core.FreeVars
import Core.Renaming

import Utilities

import Algebra.Lattice

import qualified Data.Map as M
import qualified Data.Set as S


data WhyLive = PhantomLive | ConcreteLive
             deriving (Eq, Show)

instance Pretty WhyLive where
    pPrint = text . show

instance JoinSemiLattice WhyLive where
    ConcreteLive `join` _            = ConcreteLive
    _            `join` ConcreteLive = ConcreteLive
    _            `join` _            = PhantomLive

instance BoundedJoinSemiLattice WhyLive where
    bottom = PhantomLive

newtype Liveness = Liveness { unLiveness :: M.Map (Out Var) WhyLive }
                 deriving (Eq, JoinSemiLattice, BoundedJoinSemiLattice)

mkConcreteLiveness, mkPhantomLiveness :: FreeVars -> Liveness
mkConcreteLiveness fvs = Liveness $ setToMap ConcreteLive fvs
mkPhantomLiveness fvs = Liveness $ setToMap PhantomLive fvs

-- | Warning: you almost never actually want to use this function, since this function also reports free variables of phantoms.
livenessAllFreeVars :: Liveness -> FreeVars
livenessAllFreeVars = M.keysSet . unLiveness

emptyLiveness :: Liveness
emptyLiveness = bottom

plusLiveness :: Liveness -> Liveness -> Liveness
plusLiveness = join

plusLivenesses :: [Liveness] -> Liveness
plusLivenesses = joins

whyLive :: Out Var -> Liveness -> Maybe WhyLive
whyLive x' live = x' `M.lookup` unLiveness live


inFreeVars :: (a -> FreeVars) -> In a -> FreeVars
inFreeVars thing_fvs (rn, thing) = renameFreeVars rn (thing_fvs thing)

heapBindingLiveness :: HeapBinding -> Liveness
heapBindingLiveness Environmental   = emptyLiveness
heapBindingLiveness (Updated _ fvs) = mkPhantomLiveness fvs
heapBindingLiveness (Phantom in_e)  = mkPhantomLiveness  (inFreeVars annedTermFreeVars in_e)
heapBindingLiveness (Concrete in_e) = mkConcreteLiveness (inFreeVars annedTermFreeVars in_e)

pureHeapBoundVars :: PureHeap -> BoundVars
pureHeapBoundVars = M.keysSet . M.filter (not . heapBindingNonConcrete)

stackBoundVars :: Stack -> BoundVars
stackBoundVars = S.unions . map stackFrameBoundVars

stackFrameBoundVars :: StackFrame -> BoundVars
stackFrameBoundVars = fst . stackFrameOpenFreeVars

-- FIXME: this comment is historical?
--
-- Note [Free variables of phantom bindings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Once upon a time, this function would not consider the free variables of phantom bindings to be free.
-- This has changed for two reasons:
--   1) If we want the matcher to "look through" into phantom bindings, it needs to be able to rename
--      the free variables of those bindings when it ties back. For example, consider:
--         xl |-> <yl : ysl> `match` xr |-> <yr : ysr>
--
--      When we tie back, we need to rename yl |-> yr, ysl |-> ysr. We can't do that unless the tieback
--      was abstracted over them in the first place, so we better report them as free here.
--
--      (I've later found that we can't do this anyway - see matchHeapBinding in Match.hs - so this point is moot)
--
--   2) This point is the real killer. We have a scheme to avoid duplicating values by adding values to
--      the phantom heap and then letting the evaluator "look through" into phantom bindings as long
--      as they contain values. However, if we do this in the course of evaluation a free variables
--      of a phantom binding might become a real free variable. For example:
--             < x |-> <Just y> | case x of Just z -> z | \epsilon >
--         --> < x |-> <Just y> | y | \epsilon >
--
--      So we better have bound that y above, or we're screwed!
--
-- If we only cared about 2), we could just make the free variables of phantom *values* show up as free variables,
-- but it's simpler to just make all phantom free variables into free variables.
--
-- NB: there is at least one other option:
--  1. Revert to the old pureHeapOpenFreeVars call that does the naive thing
--  2. In the *splitter*, add a phantom (e.g. Environmental) heap binding to the outgoing heaps for every
--     free variable referred to only by other phantom heap bindings
--
-- The advantage of this scheme is that we will lambda-abstract over less stuff. It doesn't even mean that
-- the supercompiled terms will be less reusable. In particular, if we had something like:
--   < x |-> <Just y> | case x of Just z -> z | \epsilon >
--
-- In the old scheme we would reduce this to just "y". You might then think that we could reuse this for any
-- new state of the same form. However, the matcher doesn't look through phantom heap bindings (even though
-- we could in this case), so we wouldn't have reused it! Thus, changing the input state to be:
--   < y |-> <>, x |-> <Just y> | case x of Just z -> z | \epsilon >
--
-- Means no change to the reusability.

stackFrameOpenFreeVars :: StackFrame -> (BoundVars, FreeVars)
stackFrameOpenFreeVars kf = case kf of
    Apply x'                -> (S.empty, annedFreeVars x')
    Scrutinise in_alts      -> (S.empty, inFreeVars annedAltsFreeVars in_alts)
    PrimApply _ in_vs in_es -> (S.empty, S.unions (map (inFreeVars annedValueFreeVars) in_vs) `S.union` S.unions (map (inFreeVars annedTermFreeVars) in_es))
    Update x'               -> (S.singleton (annee x'), S.empty)

-- | Returns the free variables that the state would have if it were residualised right now (i.e. variables bound by phantom bindings are in the free vars set)
stateFreeVars :: State -> FreeVars
stateFreeVars = snd . stateStaticBindersAndFreeVars

stateStaticBinders :: State -> BoundVars
stateStaticBinders = fst . stateStaticBindersAndFreeVars

-- | Returns the free variables that the state would have if it were residualised right now (i.e. excludes static binders),
-- along with the static binders as a separate set.
stateStaticBindersAndFreeVars :: State -> (BoundVars, FreeVars)
stateStaticBindersAndFreeVars (Heap h _, k, in_e) = (bvs_static', fvs' S.\\ bvs_nonstatic')
  where
    ((bvs_static', bvs_nonstatic'), fvs') = pureHeapOpenFreeVars h (stackOpenFreeVars k (inFreeVars annedTermFreeVars in_e))
    
    pureHeapOpenFreeVars :: PureHeap -> (BoundVars, FreeVars) -> ((BoundVars, BoundVars), FreeVars)
    pureHeapOpenFreeVars h (bvs, fvs) = M.foldWithKey (\x' hb ((bvs_static, bvs_nonstatic), fvs) -> case hb of Concrete in_e -> ((bvs_static, S.insert x' bvs_nonstatic), fvs `S.union` inFreeVars annedTermFreeVars in_e); _ -> ((S.insert x' bvs_static, bvs_nonstatic), fvs)) ((S.empty, bvs), fvs) h
    
    stackOpenFreeVars :: Stack -> FreeVars -> (BoundVars, FreeVars)
    stackOpenFreeVars k fvs = (S.unions *** (S.union fvs . S.unions)) . unzip . map stackFrameOpenFreeVars $ k
