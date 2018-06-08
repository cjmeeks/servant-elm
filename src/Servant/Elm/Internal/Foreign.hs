{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Internal.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Text
--import           Elm             (ElmDatatype, ElmType, toElmType)
import           Elm.TyRender    (ElmRenderable(..))
import           Elm.TyRep       (EType, ETypeDef, IsElmDefinition(..))
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

data ElmThing = ElmType ETypeDef | ElmConstructor EType

instance (IsElmDefinition a) => HasForeignType LangElm ElmThing a where
  typeFor _ _ _ = ElmType $
    compileElmDef (Proxy :: Proxy a)

-- instance HasForeignType LangElm ElmThing EType where
--   typeFor _ _ x = ElmConstructor x



getEndpoints
  :: ( HasForeign LangElm ElmThing api
     , GenerateList ElmThing (Foreign ElmThing api))
  => Proxy api
  -> [Req ElmThing]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmThing)
