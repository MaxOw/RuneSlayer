{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}
{-# Language PatternSynonyms #-}
module Dhall.Fix
    ( InterpretFix
    , autoWithFix
    ) where

import Relude hiding (Type, Const)
import Dhall hiding (maybe, embed)
import Dhall.Core hiding (Type)
import qualified Dhall.Core
import Dhall.Src (Src)
import Dhall.TypeCheck (X)
import GHC.TypeLits
import Data.Functor.Foldable (Base, Corecursive, elgot, embed)
import Control.Monad.Trans.Free

--------------------------------------------------------------------------------

type InterpretFix f t =
   ( Traversable f
   , Corecursive t
   , Interpret (f (FixVar "t"))
   , Base t ~ f
   )

autoWithFix :: forall f t. InterpretFix f t => InterpretOptions -> Type t
autoWithFix opts = unDhallFix <$> autoWith @(DhallFix f t) opts

--------------------------------------------------------------------------------

newtype DhallFix f t = DhallFix { unDhallFix :: t }
newtype FixVar (v :: Symbol) = FixVar { unFreeVar :: (Expr Src X) }

instance KnownSymbol v => Interpret (FixVar v) where
    autoWith _ = Type
        { extract  = \case App "fix" e0 -> Just (FixVar e0); _ -> Nothing
        , expected = fromString $ symbolVal @v Proxy
        }

instance InterpretFix f t => Interpret (DhallFix f t) where
    autoWith opts = Type { extract = extr, expected = expe }
        where
        -- TODO: handle alpha-renaming/shadowing & allow other names for "fix"
        extr (Lam _ _ (Lam "fix" _ e)) = DhallFix <$> buildF e
        extr _                         = Nothing

        buildF = elgot f g <=< extract (auto @(FixVar "t"))

        g = maybe (Left Nothing) (Right . Free) . extract xF_t . unFreeVar
        f = \case Free x -> embed <$> sequenceA x; _ -> Nothing

        expe = Pi "t" CType                         -- = ∀(t : Type)
             $ Pi "_" (Pi "_" (expected xF_t) "t")  -- → ∀(F t -> t)
             $ "t"                                  -- → t

        xF_t = autoWith @(f (FixVar "t")) opts

pattern CType :: Expr s a
pattern CType = Const Dhall.Core.Type

