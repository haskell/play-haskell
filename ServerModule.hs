{-# LANGUAGE ExistentialQuantification #-}
module ServerModule where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.String (fromString)
import Snap.Core


-- TODO: Perhaps this can be split out over modules as well
data Options = Options { oProxied :: Bool
                       , oDBDir :: FilePath }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options False "."

type MimeType = String

data ServerModule =
    forall ctx req. ServerModule
        { smMakeContext :: Options -> (ctx -> IO ()) -> IO ()  -- bracket
        , smParseRequest :: Method -> ByteString -> Maybe req
        , smHandleRequest :: ctx -> req -> Snap ()
        , smStaticFiles :: [(FilePath, MimeType)] }


httpError :: Int -> String -> Snap ()
httpError code msg = do
    putResponse $ setResponseCode code emptyResponse
    writeBS (Char8.pack msg)

applyStaticFileHeaders :: String -> Response -> Response
applyStaticFileHeaders mime =
    setContentType (Char8.pack mime)
    . setHeader (fromString "Cache-Control") (Char8.pack "public max-age=3600")
