module Main (main) where

import Control.Monad
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.FilePath ((</>))
import System.IO

import qualified Options as Opt
import Paste
import ServerModule


data InstantiatedModule = InstantiatedModule
    { imHandler :: Method -> ByteString -> Maybe (Snap ())
    , imStatics :: Map ByteString MimeType }

instantiate :: Options -> ServerModule -> (InstantiatedModule -> IO ()) -> IO ()
instantiate options m k =
    case m of
      ServerModule { smMakeContext = make
                   , smParseRequest = parse
                   , smHandleRequest = handle
                   , smStaticFiles = statics } ->
          make options $ \ctx ->
              k $ InstantiatedModule
                    { imHandler = \method path ->
                                      (\req -> handle ctx req) <$> parse method path
                    , imStatics = Map.fromList (map (first Char8.pack) statics) }

instantiates :: Options -> [ServerModule] -> ([InstantiatedModule] -> IO ()) -> IO ()
instantiates _ [] f = f []
instantiates options (m : ms) f =
    instantiate options m $ \m' ->
        instantiates options ms $ \ms' ->
            f (m' : ms')

staticFile :: String -> FilePath -> Snap ()
staticFile mime path = do
    modifyResponse (applyStaticFileHeaders mime)
    sendFile ("static" </> path)

handleStaticFiles :: [InstantiatedModule] -> ByteString -> Maybe (Snap ())
handleStaticFiles _ path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
  = Nothing
handleStaticFiles ms path =
    case BS.split (fromIntegral (ord '/')) (trimSlashes path) of 
      [component] -> case mapMaybe (\m -> Map.lookup component (imStatics m)) ms of
                       [mime] -> Just (staticFile mime (Char8.unpack component))
                       [] -> Nothing
                       _ -> error $ "Multiple handlers for the same static file:" ++ show component
      _ -> Nothing
  where
    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

server :: Options -> [InstantiatedModule] -> Snap ()
server options modules = do
    -- If we're proxied, set the source IP from the X-Forwarded-For header.
    when (oProxied options) ipHeaderFilter

    req <- getRequest
    let path = rqContextPath req `BS.append` rqPathInfo req
        method = rqMethod req

    fromMaybe (httpError 404 "Page not found") $ asum $
        [guard (method == GET) >> handleStaticFiles modules path]
        ++ map (\m -> imHandler m method path) modules

config :: Config Snap a
config =
    let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
    in setAccessLog stderrlogger
       . setErrorLog stderrlogger
       . setPort 8123
       $ defaultConfig

main :: IO ()
main = do
    options <- Opt.parseOptions $ Opt.Interface defaultOptions $ Map.fromList
        [("--proxied", Opt.Flag "Assumes the server is running behind a proxy that sets \
                                \X-Forwarded-For, instead of using the source IP of a \
                                \request for rate limiting."
                                (\o -> o { oProxied = True }))
        ,("--dbdir", Opt.Setter "Sets directory to store pastes.db in."
                                (\o s -> o { oDBDir = s }))
        ,("--help", Opt.Help)
        ,("-h", Opt.Help)]

    let modules = [pasteModule]
    instantiates options modules $ \modules' ->
        httpServe config (server options modules')
