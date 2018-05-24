{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Elm.Internal.Orphans where

--import           Elm         (ElmDatatype, ElmType, toElmType)
import           Elm.Derive (SumEncoding(..), deriveElmDef, defaultOptions)
import           Elm.TyRep (ESum(..), EType(..), ETypeDef(..), ETypeName(..), ETVar(..), IsElmDefinition(..), SumEncoding'(..))

import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Servant.API (NoContent, Headers, getResponse)


-- instance IsElmDefinition ETypeDef where
--   compileElmDef _ = compileElmDef (Proxy :: Proxy ETypeDef)




-- instance IsElmDefinition NoContent

deriveElmDef defaultOptions ''Text

deriveElmDef defaultOptions ''NoContent

instance IsElmDefinition a => (IsElmDefinition [a]) where
  compileElmDef _ = ETypeSum (ESum {es_name = ETypeName {et_name = "List", et_args = [ETVar {tv_name = "a"}]}, es_options = [("List",Right [ETyVar (ETVar {tv_name = "a"})])], es_type = SumEncoding' ObjectWithSingleField, es_omit_null = False, es_unary_strings = True})


-- TODO: Generate Elm functions that can handle the response headers. PRs
-- welcome!
-- instance (IsElmDefinition a) => IsElmDefinition (Headers ls a) where
--   compileElmDef _ = compileElmDef (Proxy :: Proxy a)
