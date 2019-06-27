let types = ./Types.dhall

let ItemUseEffect = types.ItemUseEffect
let ItemTypeName  = types.ItemTypeName

let transformInto =
  λ(x : ItemTypeName) →
    ItemUseEffect.TransformInto { TransformInto = x }

let heal =
  λ(x : Natural) →
    ItemUseEffect.Heal { Heal = x }
in
{ transformInto = transformInto
, heal          = heal
}

