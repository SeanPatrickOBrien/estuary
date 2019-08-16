{-# LANGUAGE TemplateHaskell #-}
module Estuary.Languages.CineCer0.TH where

import Control.Lens
import Control.Lens.TH

data SomeType = SomeType { _someField :: Int }

$(makeLenses ''SomeType)



-- makeLenses :: Name -> Q [Dec]
-- lookupTypeName :: String -> Q (Maybe Name)
-- lensHack :: String -> Q [Dec]
-- lensHack x = do
--   y <- lookupTypeName x
-- maybe (return []) makeLenses y
