{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import Elm.Derive
import Elm.TyRep (ESum(..), EType(..), ETypeDef(..), ETypeName(..), ETVar(..), IsElmDefinition(..), SumEncoding'(..))
import Elm.Versions (ElmVersion(..))
import Elm.Module

import Data.List (intersperse)
import Data.Map (Map(..))
import Data.Proxy
import Data.Text (Text, pack, unpack)

import           GHC.Generics (Generic)
import           Servant.API  ((:>), (:<|>), Capture, Get, JSON)

import Servant.Elm.Internal.Generate

import Servant.Elm.Internal.Foreign
import Servant.Elm.Internal.Orphans

import Text.Pretty.Simple

type BooksApi
   = "books" :>
   (Capture "id" Text :> Get '[ JSON] Foo
    :<|> Get '[JSON] [Foo]
    :<|> "dict" :> Get '[JSON] (Map Text Foo))

data Foo
   = Foo
   { f_name :: String
   , f_text :: Text
   , f_gender :: Gender
   , f_list :: [Int]
   } deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

deriveBoth defaultOptions ''Gender

deriveElmDef defaultOptions ''Foo

main :: IO ()
main = do
    generateElmModuleWith defElmOptions "Main" defElmImports "Main.elm" 
        [ DefineElm (Proxy :: Proxy Foo)
        , DefineElm (Proxy :: Proxy Gender)
        ]
        (Proxy :: Proxy BooksApi)
