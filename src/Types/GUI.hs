{-# Language TemplateHaskell #-}
module Types.GUI where

import Delude

import Engine.Layout.Types

--------------------------------------------------------------------------------

data MenuBoxOpts = MenuBoxOpts
   { menuBoxOpts_size  :: Size Sizing
   , menuBoxOpts_title :: Text
   }
makeFieldsCustom ''MenuBoxOpts

instance Default MenuBoxOpts where
    def = MenuBoxOpts
        { menuBoxOpts_size  = pure (1 @@ cpct)
        , menuBoxOpts_title = ""
        }

