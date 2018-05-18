{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Internal.Foreign where

import           Data.Proxy      (Proxy (Proxy))
--import           Elm             (ElmDatatype, ElmType, toElmType)
import           Elm.TyRep       (ETypeDef, IsElmDefinition(..))
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

instance (IsElmDefinition a) => HasForeignType LangElm ETypeDef a where
  typeFor _ _ _ =
    compileElmDef (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangElm ETypeDef api
     , GenerateList ETypeDef (Foreign ETypeDef api))
  => Proxy api
  -> [Req ETypeDef]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ETypeDef)
