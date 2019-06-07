module Benchmarks (runBenchmarks) where

import Relude
import Criterion.Main

import Benchmarks.Scroller

runBenchmarks :: IO ()
runBenchmarks = defaultMain
    [ bench_scroller
    ]
