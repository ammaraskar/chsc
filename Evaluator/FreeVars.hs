{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
module Evaluator.FreeVars (
    inFreeVars,
    heapBindingReferences, heapBindingFreeVars,
    pureHeapBoundVars, stackBoundVars, stackFrameBoundVars,
    stateFreeVars, stateStaticBinders, stateStaticBindersAndFreeVars
  ) where

import Evaluator.Syntax

import Core.FreeVars
import Core.Renaming

import Utilities

import qualified Data.Map as M
import qualified Data.Set as S


inFreeVars :: (a -> FreeVars) -> In a -> FreeVars
inFreeVars thing_fvs (rn, thing) = renameFreeVars rn (thing_fvs thing)

-- | Finds the set of things "referenced" by a 'HeapBinding': this is only used to construct tag-graphs
heapBindingReferences :: HeapBinding -> FreeVars
heapBindingReferences Environmental   = S.empty
heapBindingReferences (Updated _ fvs) = fvs
heapBindingReferences (Phantom in_e)  = inFreeVars annedTermFreeVars in_e
heapBindingReferences (Concrete in_e) = inFreeVars annedTermFreeVars in_e

-- NB: reporting the FVs of an Updated thing as live is bad. In particular:
--  1) It causes us to abstract over too many free variables, because transitiveInline will pull in
--     things the update frame "references" as phantom bindings, even if they are otherwise dead.
--  2) It causes assert failures in the matcher because we find ourselves unable to rename the free
--     variables of the phantom bindings thus pulled in (they are dead, so the matcher doesn't get to them)
heapBindingFreeVars :: HeapBinding -> FreeVars
heapBindingFreeVars hb = case heapBindingTerm hb of
    Nothing                -> S.empty
    Just (in_e, _why_live) -> inFreeVars annedTermFreeVars in_e

-- | Returns all the variables bound by the heap that we might have to residualise in the splitter
pureHeapBoundVars :: PureHeap -> BoundVars
pureHeapBoundVars = M.keysSet -- I think its harmless to include variables bound by phantoms in this set

-- | Returns all the variables bound by the stack that we might have to residualise in the splitter
stackBoundVars :: Stack -> BoundVars
stackBoundVars = S.unions . map stackFrameBoundVars -- It's *vital* to include variables bound by phantoms in this set

stackFrameBoundVars :: StackFrame -> BoundVars
stackFrameBoundVars kf = bvs_static `S.union` bvs_nonstatic
  where (bvs_static, bvs_nonstatic) = fst (stackFrameOpenFreeVars kf)

stackFrameOpenFreeVars :: StackFrame -> ((BoundVars, BoundVars), FreeVars)
stackFrameOpenFreeVars kf = case kf of
    Apply x'                -> ((S.empty, S.empty), annedFreeVars x')
    Scrutinise in_alts      -> ((S.empty, S.empty), inFreeVars annedAltsFreeVars in_alts)
    PrimApply _ in_vs in_es -> ((S.empty, S.empty), S.unions (map (inFreeVars annedValueFreeVars) in_vs) `S.union` S.unions (map (inFreeVars annedTermFreeVars) in_es))
    Update x' why_live      -> (case why_live of ConcreteLive -> (S.empty, S.singleton (annee x')); PhantomLive -> (S.singleton (annee x'), S.empty), S.empty)

-- | Returns (an overapproximation of) the free variables that the state would have if it were residualised right now (i.e. variables bound by phantom bindings *are* in the free vars set)
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
    
    pureHeapOpenFreeVars :: PureHeap -> ((BoundVars, BoundVars), FreeVars) -> ((BoundVars, BoundVars), FreeVars)
    pureHeapOpenFreeVars h (bvs, fvs) = M.foldWithKey one (bvs, fvs) h
      where
        one x' (Concrete in_e) ((bvs_static, bvs_nonstatic), fvs) = ((bvs_static, S.insert x' bvs_nonstatic), fvs `S.union` inFreeVars annedTermFreeVars in_e)
        one x' (Phantom  in_e) ((bvs_static, bvs_nonstatic), fvs) = ((S.insert x' bvs_static, bvs_nonstatic), fvs `S.union` inFreeVars annedTermFreeVars in_e) -- FVs of phantoms can become free after reduction
        one x' _               ((bvs_static, bvs_nonstatic), fvs) = ((S.insert x' bvs_static, bvs_nonstatic), fvs)
    
    stackOpenFreeVars :: Stack -> FreeVars -> ((BoundVars, BoundVars), FreeVars)
    stackOpenFreeVars k fvs = (((S.unions *** S.unions) . unzip) *** (S.union fvs . S.unions)) . unzip . map stackFrameOpenFreeVars $ k
