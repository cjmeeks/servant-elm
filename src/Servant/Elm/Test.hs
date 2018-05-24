{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import Elm.Derive
import Elm.TyRep (ESum(..), EType(..), ETypeDef(..), ETypeName(..), ETVar(..), IsElmDefinition(..), SumEncoding'(..))
import Elm.Module

import Data.Proxy
import Data.Text (Text, pack, unpack)

import           GHC.Generics (Generic)
import           Servant.API  ((:>), (:<|>), Capture, Get, JSON)

import Servant.Elm.Internal.Generate

import Servant.Elm.Internal.Foreign
import Servant.Elm.Internal.Orphans

import Text.Pretty.Simple

type BooksApi = "books" :> (Capture "id" Text :> Get '[JSON] Foo
    :<|> Get '[JSON] [Foo])

data Foo
   = Foo
   { f_name :: String
   , f_text :: Text
   , f_gender :: Gender
   , f_other :: List String
   , f_list :: [Int]
   } deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data List a = List {
     inner :: a
    } deriving (Show, Eq)

data NoContent = NoContent

data Maybee = Maybee
  { m_a :: Maybe Int

    }

newtype MyText = MyText Text

deriveElmDef defaultOptions ''MyText

deriveElmDef defaultOptions ''Maybee

-- newtype Wtf a = Wtf {
--   getWtf :: a
--   } deriving (Show, Eq)

--deriveElmDef defaultOptions ''(Wtf Int)

deriveElmDef defaultOptions ''NoContent

deriveBoth defaultOptions ''Gender

deriveBoth defaultOptions ''Foo

deriveBoth defaultOptions ''List

--deriveElmDef defaultOptions ''[Foo]

--deriveElmDef defaultOptions ''Maybe

main :: IO ()
main = do
    putStrLn $ makeElmModule "Foo"
        [ DefineElm (Proxy :: Proxy Foo)
        , DefineElm (Proxy :: Proxy Gender)
        , DefineElm (Proxy :: Proxy (List Int))
        ]
    mapM_ (putStrLn . unpack) $ generateElmForAPI (Proxy :: Proxy BooksApi)
