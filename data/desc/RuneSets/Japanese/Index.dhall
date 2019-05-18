
let makeHiraganaEntry =
  λ(l : Natural) →
  λ(n : Text) →
  λ(r : Text) →
    { name    = n
    , kind    = "hiragana"
    , level   = l
    , query   = "Reading of ${n}?"
    , answers = [r]
    -- , validation = "Exact"
    }

in
-- [ makeHiraganaGroup 1 "" ["a", "i", "u", "e", "o"]
[ makeHiraganaEntry 1 "あ" "a"
, makeHiraganaEntry 1 "い" "i"
, makeHiraganaEntry 1 "う" "u"
, makeHiraganaEntry 1 "え" "e"
, makeHiraganaEntry 1 "お" "o"
]
