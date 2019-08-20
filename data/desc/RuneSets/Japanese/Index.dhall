
let makeHiraganaEntry =
  λ(l : Natural) →
  λ(q : Text) →
  λ(r : Text) →
    { name    = "hiragana_${r}"
    , kind    = "hiragana"
    , level   = l
    , query   = "Reading of ${q}?"
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
