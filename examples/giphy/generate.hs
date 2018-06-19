{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import qualified Data.Text    as T
import           Elm.Derive   (defaultOptions, deriveBoth)
import qualified Elm.Derive as Elm

import           Servant.API  ((:>), Capture, Get, JSON, QueryParam)
import           Servant.Elm  (DefineElm (DefineElm), ElmOptions (urlPrefix), Proxy (Proxy), UrlPrefix (Static), defElmImports, defElmOptions,
                               generateElmModuleWith)

data GifData = GifData
  { image_url :: String
  } deriving (Show, Eq)

data Gif = Gif
  { _data :: GifData
  } deriving (Show, Eq)

concat <$> mapM
  (deriveBoth Elm.defaultOptions
    { Elm.fieldLabelModifier = \ field ->
        if head field == '_' then
          tail field
        else
          field
    }
  ) [''GifData, ''Gif]

myElmOpts :: ElmOptions
myElmOpts =
  defElmOptions
    { urlPrefix =
        Static "http://api.giphy.com/v1/gifs"
    }

type GiphyApi = "random" :> QueryParam "api_key" String :> QueryParam "tag" String :> Get '[JSON] Gif

main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "GiphyApi"
    ]
    defElmImports
    "elm"
    [ DefineElm (Proxy :: Proxy Gif)
    , DefineElm (Proxy :: Proxy GifData)
    ]
    (Proxy :: Proxy GiphyApi)
