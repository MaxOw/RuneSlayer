module Types.GUI where

import Delude

import Engine.Layout.Types

--------------------------------------------------------------------------------

data MenuBoxOpts = MenuBoxOpts
   { field_size  :: Size Sizing
   , field_title :: Text
   } deriving (Generic)


instance Default MenuBoxOpts where
    def = MenuBoxOpts
        { field_size  = pure (1 @@ cpct)
        , field_title = ""
        }

