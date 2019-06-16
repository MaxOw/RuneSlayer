let types = ./Types.dhall

let ItemUseEffect = types.ItemUseEffect

let transformInto =
  λ(x : Text) →
    ItemUseEffect.TransformInto { TransformInto = x }

let heal =
  λ(x : Natural) →
    ItemUseEffect.Heal { Heal = x }
in
{ transformInto = transformInto
, heal          = heal
}

