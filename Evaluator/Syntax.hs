{-# LANGUAGE TypeOperators #-}
module Evaluator.Syntax where

import Evaluator.Deeds

import Core.FreeVars
import Core.Renaming
import Core.Size
import Core.Syntax
import Core.Tag

import Renaming
import Utilities

import qualified Data.Map as M


type Anned = Tagged :.: Sized :.: FVed
type AnnedTerm = Anned (TermF Anned)
type AnnedValue = ValueF Anned
type AnnedAlt = AltF Anned

annee :: Anned a -> a
annee = extract

annedSize :: Anned a -> Size
annedSize = size . unComp . tagee . unComp

annedFreeVars :: Anned a -> FreeVars
annedFreeVars = freeVars . sizee . unComp . tagee . unComp

annedTag :: Anned a -> Tag
annedTag = tag . unComp


annedVarFreeVars' = taggedSizedFVedVarFreeVars'
annedTermFreeVars = taggedSizedFVedTermFreeVars
annedTermFreeVars' = taggedSizedFVedTermFreeVars'
annedValueFreeVars = taggedSizedFVedValueFreeVars
annedValueFreeVars' = taggedSizedFVedValueFreeVars'
annedAltsFreeVars = taggedSizedFVedAltsFreeVars

annedVarSize' = taggedSizedFVedVarSize'
annedTermSize' = taggedSizedFVedTermSize'
annedTermSize = taggedSizedFVedTermSize
annedValueSize' = taggedSizedFVedValueSize'
annedValueSize = taggedSizedFVedValueSize
annedAltsSize = taggedSizedFVedAltsSize

renameAnnedTerm = renameTaggedSizedFVedTerm :: IdSupply -> Renaming -> AnnedTerm -> AnnedTerm
renameAnnedValue = renameTaggedSizedFVedValue
renameAnnedValue' = renameTaggedSizedFVedValue'
renameAnnedAlts = renameTaggedSizedFVedAlts

detagAnnedTerm = taggedSizedFVedTermToFVedTerm
detagAnnedValue = taggedSizedFVedValueToFVedValue
detagAnnedValue' = taggedSizedFVedValue'ToFVedValue'
detagAnnedAlts = taggedSizedFVedAltsToFVedAlts


annedVar :: Tag -> Var -> Anned Var
annedVar   tg x = Comp (Tagged tg (Comp (Sized (annedVarSize' x)   (FVed (annedVarFreeVars' x)  x))))

annedTerm :: Tag -> TermF Anned -> AnnedTerm
annedTerm  tg e = Comp (Tagged tg (Comp (Sized (annedTermSize' e)  (FVed (annedTermFreeVars' e)  e))))

annedValue :: Tag -> ValueF Anned -> Anned AnnedValue
annedValue tg v = Comp (Tagged tg (Comp (Sized (annedValueSize' v) (FVed (annedValueFreeVars' v) v))))


toAnnedTerm :: Term -> AnnedTerm
toAnnedTerm = tagFVedTerm . reflect


data QA = Question Var
        | Answer   (ValueF Anned)

instance Pretty QA where
    pPrintPrec level prec = pPrintPrec level prec . qaToAnnedTerm'

qaToAnnedTerm' :: QA -> TermF Anned
qaToAnnedTerm' (Question x) = Var x
qaToAnnedTerm' (Answer v)   = Value v


type UnnormalisedState = (Deeds, Heap, Stack, In AnnedTerm)
type State = (Deeds, Heap, Stack, In (Anned QA))

denormalise :: State -> UnnormalisedState
denormalise (deeds, h, k, (rn, qa)) = (deeds, h, k, (rn, fmap qaToAnnedTerm' qa))


-- Invariant: LetBound things cannot refer to LambdaBound things.
--
-- This is motivated by:
--  1. There is no point lambda-abstracting over things referred to by LetBounds because the resulting h-function would be
--     trapped under the appropriate let-binding anyway, at which point all the lambda-abstracted things would be in scope as FVs.
--  2. It allows (but does not require) the matcher to look into the RHS of LetBound stuff (rather than just doing nominal
--     matching).
data HowBound = InternallyBound | LambdaBound | LetBound
              deriving (Eq, Show)

instance Pretty HowBound where
    pPrint = text . show

instance NFData HowBound

data HeapBinding = HB { howBound :: HowBound, heapBindingTag :: Maybe Tag, heapBindingTerm :: Maybe (In AnnedTerm) }
                 deriving (Show)

instance NFData HeapBinding where
    rnf (HB a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Heap where
    rnf (Heap a b) = rnf a `seq` rnf b

instance Pretty HeapBinding where
    pPrintPrec level prec (HB how _ mb_in_e) = case how of
        InternallyBound -> maybe empty (pPrintPrec level prec) mb_in_e
        LambdaBound     -> text "λ" <> angles (maybe empty (pPrintPrec level noPrec) mb_in_e)
        LetBound        -> text "l" <> angles (maybe empty (pPrintPrec level noPrec) mb_in_e)

internallyBound :: In AnnedTerm -> HeapBinding
internallyBound in_e@(_, e) = HB InternallyBound (Just (annedTag e)) (Just in_e)

environmentallyBound :: HeapBinding
environmentallyBound = HB LetBound Nothing Nothing

type PureHeap = M.Map (Out Var) HeapBinding
data Heap = Heap PureHeap IdSupply
          deriving (Show)

instance Pretty Heap where
    pPrintPrec level prec (Heap h _) = pPrintPrec level prec h


type Stack = [Tagged StackFrame]
data StackFrame = Apply (Out Var)
                | Scrutinise (In [AnnedAlt])
                | PrimApply PrimOp [In (Anned AnnedValue)] [In AnnedTerm]
                | Update (Out Var)
                deriving (Show)

instance NFData StackFrame where
    rnf (Apply a)         = rnf a
    rnf (Scrutinise a)    = rnf a
    rnf (PrimApply a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Update a)        = rnf a

instance Pretty StackFrame where
    pPrintPrec level prec kf = case kf of
        Apply x'                  -> pPrintPrecApp level prec (text "[_]") x'
        Scrutinise in_alts        -> pPrintPrecCase level prec (text "[_]") (renameIn (renameAnnedAlts prettyIdSupply) in_alts)
        PrimApply pop in_vs in_es -> pPrintPrecPrimOp level prec pop (map SomePretty in_vs ++ map SomePretty in_es)
        Update x'                 -> pPrintPrecApp level prec (text "update") x'


-- | Size of HeapBinding for Deeds purposes
heapBindingSize :: HeapBinding -> Size
heapBindingSize (HB InternallyBound _ (Just (_, e))) = annedSize e
heapBindingSize _                                    = 0

-- | Size of StackFrame for Deeds purposes
stackFrameSize :: StackFrame -> Size
stackFrameSize kf = 1 + case kf of
    Apply _                 -> 0
    Scrutinise (_, alts)    -> annedAltsSize alts
    PrimApply _ in_vs in_es -> sum (map (annedValueSize . snd) in_vs ++ map (annedTermSize . snd) in_es)
    Update _                -> 0


-- Used by the GC logic, so the first renaming will be partial
renameHeapBinding :: Renaming -> HeapBinding -> HeapBinding
renameHeapBinding rn hb = hb { heapBindingTerm = liftM (renameInRenaming rn) (heapBindingTerm hb) }

-- Used by the GC logic, so the first renaming will be partial
renameStackFrame :: Renaming -> StackFrame -> StackFrame
renameStackFrame rn kf = case kf of
    Apply x'                  -> Apply (rename_maybe rn x' `orElse` x')
    Scrutinise in_alts        -> Scrutinise (renameInRenaming rn in_alts)
    PrimApply pop in_vs in_es -> PrimApply pop (map (renameInRenaming rn) in_vs) (map (renameInRenaming rn) in_es)
    Update x'                 -> Update (rename_maybe rn x' `orElse` x')


addStateDeeds :: Deeds -> (Deeds, Heap, Stack, In (Anned a)) -> (Deeds, Heap, Stack, In (Anned a))
addStateDeeds extra_deeds (deeds, h, k, in_e) = (extra_deeds + deeds, h, k, in_e)

releaseHeapBindingDeeds :: Deeds -> HeapBinding -> Deeds
releaseHeapBindingDeeds deeds hb = deeds + heapBindingSize hb

releasePureHeapDeeds :: Deeds -> PureHeap -> Deeds
releasePureHeapDeeds = M.fold (flip releaseHeapBindingDeeds)

releaseStackDeeds :: Deeds -> Stack -> Deeds
releaseStackDeeds = foldl' (\deeds kf -> deeds + stackFrameSize (tagee kf))

releaseStateDeed :: (Deeds, Heap, Stack, In (Anned a)) -> Deeds
releaseStateDeed (deeds, Heap h _, k, (_, e)) = releaseStackDeeds (releasePureHeapDeeds (deeds + annedSize e) h) k
