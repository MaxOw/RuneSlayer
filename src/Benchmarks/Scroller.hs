{-# OPTIONS_GHC -fno-warn-orphans #-}
module Benchmarks.Scroller (bench_scroller) where

import Delude
import Criterion.Main

import Engine.Benchmark (mkBench)
import Types.St
import Game (initSt)
import Engine (Engine)
-- import Engine.Debug
-- import Engine.Common.Types (mkBBoxCenter)
-- import EntityIndex (lookupInRange)
-- import Types.Entity.Common
import View (prerenderUpdate)

instance NFData St where rnf _ = ()

bench_scroller :: Benchmark
bench_scroller = mkBench "scroller" initSt runScrollerUpdate

{-
runTileLookup :: St -> Engine St ()
runTileLookup st = do
    s <- liftIO . readIORef $ st^.scroller._Wrapped
    let scrollerSize = s^.size
    let viewScale = st^.gameState.gameScale
    let bb = mkBBoxCenter (pure 0) (scrollerSize ^/ viewScale)
    es <- lookupInRange EntityKind_Tile bb (st^.gameState.entities)
    let ls = length es
    logOnce (show ls)
    evaluateNF_ ls
-}

runScrollerUpdate :: St -> Engine St ()
runScrollerUpdate st = do
    _ <- prerenderUpdate True st
    return ()
