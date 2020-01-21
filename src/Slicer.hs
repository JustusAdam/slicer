{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedLists #-}
module Slicer (AST(..), slice) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Function

class (Ord (Node p), Enum (Node p), Show (Node p), Show (Var p)) => AST p where
    type Node p
    type Var p
    indices :: p -> [Node p]
    suc :: p -> Node p -> [Node p]
    ref :: p -> Node p -> S.Set ( Var p )
    def :: p -> Node p -> S.Set ( Var p )
    infl :: p -> Node p -> S.Set (Node p)

type C p = (Node p, S.Set (Var p))
type StmSel p = Node p -> Bool
type R p = Node p -> S.Set ( Var p )

relevancies :: (Ord (Var p), AST p) => p -> C p -> R p
relevancies program (n_0, initvars) = getVars
  where
    getVars = fromMaybe mempty . flip M.lookup m
    m = M.fromList
        [  (n,
          vars)
        | n <- indices program
        , let r_m = S.unions $ map getVars $ suc program n
              carryover = S.difference r_m (def program n)
              vars
                  | n == n_0 = initvars
                  | not $ S.null $ def program n `S.intersection` r_m = S.union (ref program n) carryover
                  | otherwise = carryover
        ]

slice :: (Ord (Var p), AST p,Show (Node p)) => p -> C p -> StmSel p
slice program criterion = fix go r_0 s_0 b_0
  where
    originalMkB s = S.unions [ infl program n | n <- S.toList s]
    myMkB s = S.fromList [ n | n <- indices program
                             , not $ S.null $ infl program n `S.intersection` s ]

    mkB = myMkB

    mkS r_1 b_i = b_i `S.union` S.fromList [n | n <- indices program, not (S.null $ def program n `S.intersection` r_1 (succ n))]

    r_0 = relevancies program criterion
    s_0 = mkS r_0 S.empty
    b_0 = mkB s_0

    go rec_ r_i s_i b_i | s_i == s_1 = flip S.member s_1
                        | otherwise = rec_ r_1 s_1 b_1
      where
        r_1 n = S.unions $ r_i n : [relevancies program (b, ref program b) n | b <- S.toList b_i]
        s_1 = mkS r_1 b_i
        b_1 = mkB s_1
