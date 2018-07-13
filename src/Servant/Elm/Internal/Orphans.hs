{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Elm.Internal.Orphans where

import           Data.Proxy (Proxy(Proxy))
import           GHC.TypeLits (KnownSymbol)
import           Servant.API.Experimental.Auth (AuthProtect)
import           Servant.Foreign ((:>), Foreign, HasForeign(..), foreignFor)

-- TODO: Add actual support for handling Auth in Elm
instance (KnownSymbol sym, HasForeign lang ftype sublayout)
    => HasForeign lang ftype (AuthProtect sym :> sublayout) where
    type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy req =
      foreignFor lang ftype (Proxy :: Proxy sublayout) req
