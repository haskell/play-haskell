module Main (main) where

import Control.Concurrent.STM
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
import System.IO
import qualified System.Posix.Signals as Signal

import qualified Options as Opt
import Paste
import Paste.DB (withDatabase)
import Pages
import Play
import ServerModule
import Shim
import SpamDetect


data InstantiatedModule = InstantiatedModule
    { imHandler :: Method -> [ByteString] -> Maybe (Snap ())
    , imStatics :: Map ByteString MimeType }

instantiate :: GlobalContext -> Options -> ServerModule -> (InstantiatedModule -> IO ()) -> IO ()
instantiate gctx options m k =
    case m of
      ServerModule { smMakeContext = make
                   , smParseRequest = parse
                   , smHandleRequest = handle
                   , smStaticFiles = statics } ->
          make gctx options $ \ctx ->
              k $ InstantiatedModule
                    { imHandler = \method path ->
                                      (\req -> handle gctx ctx req) <$> parse method path
                    , imStatics = Map.fromList (map (first Char8.pack) statics) }

instantiates :: GlobalContext -> Options -> [ServerModule] -> ([InstantiatedModule] -> IO ()) -> IO ()
instantiates _ _ [] f = f []
instantiates gctx options (m : ms) f =
    instantiate gctx options m $ \m' ->
        instantiates gctx options ms $ \ms' ->
            f (m' : ms')

splitPath :: ByteString -> Maybe [ByteString]
splitPath path
  | BS.null path || BS.head path /= fromIntegral (ord '/')
  = Nothing
splitPath path = Just (BS.split (fromIntegral (ord '/')) (trimSlashes path))
  where
    trimSlashes :: ByteString -> ByteString
    trimSlashes = let slash = fromIntegral (ord '/')
                  in BS.dropWhile (== slash) . BS.dropWhileEnd (== slash)

handleStaticFiles :: [InstantiatedModule] -> [ByteString] -> Maybe (Snap ())
handleStaticFiles ms [component] =
  case mapMaybe (\m -> Map.lookup component (imStatics m)) ms of
    [mime] -> Just (staticFile mime (Char8.unpack component))
    [] -> Nothing
    _ -> error $ "Multiple handlers for the same static file:" ++ show component
handleStaticFiles _ _ = Nothing

refreshPages :: TVar Pages -> IO ()
refreshPages var = do
    pagesFromDisk >>= atomically . writeTVar var
    putStrLn "Reloaded pages"

server :: Options -> [InstantiatedModule] -> Snap ()
server options modules = do
    -- If we're proxied, set the source IP from the X-Forwarded-For header.
    when (oProxied options) ipHeaderFilterSupportingIPv6

    req <- getRequest
    let path = rqContextPath req `BS.append` rqPathInfo req
        method = rqMethod req

    case splitPath path of
      Just components ->
        fromMaybe (httpError 404 "Page not found") $ asum $
            [guard (method == GET) >> handleStaticFiles modules components]
            ++ map (\m -> imHandler m method components) modules
      Nothing -> httpError 400 "Invalid URL"

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

    spam <- initSpamDetect
    pagesvar <- pagesFromDisk >>= newTVarIO

    _ <- Signal.installHandler Signal.sigUSR1 (Signal.Catch (refreshPages pagesvar)) Nothing

    withDatabase (oDBDir options) $ \db -> do
        let gctx = GlobalContext
                     { gcSpam = spam
                     , gcDb = db
                     , gcPagesVar = pagesvar }

        let modules = [pasteModule, playModule]
        instantiates gctx options modules $ \modules' -> do
            httpServe config (server options modules')
