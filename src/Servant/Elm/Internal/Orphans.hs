{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Elm.Internal.Orphans where

--import           Elm         (ElmDatatype, ElmType, toElmType)
import           Data.Proxy (Proxy(..))
import           Elm.TyRep       (ETypeDef, IsElmDefinition(..))
import           Servant.API (NoContent, Headers, getResponse)


instance IsElmDefinition ETypeDef where
  compileElmDef _ = compileElmDef (Proxy :: Proxy ETypeDef)


instance IsElmDefinition NoContent


-- TODO: Generate Elm functions that can handle the response headers. PRs
-- welcome!
-- instance (IsElmDefinition a) => IsElmDefinition (Headers ls a) where
--   compileElmDef _ = compileElmDef (Proxy :: Proxy a)
