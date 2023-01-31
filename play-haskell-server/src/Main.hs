module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Char (ord, isSpace)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Snap.Core hiding (path, method)
import Snap.Http.Server
import System.Exit (die)
import System.IO
import qualified System.Posix.Signals as Signal
import Text.Read (readMaybe)

import DB (withDatabase)
import Pages
import Play
import qualified PlayHaskellTypes.Sign as Sign
import ServerModule
import Snap.Server.Utils
import Snap.Server.Utils.Hex
import qualified Snap.Server.Utils.Options as Opt
import Snap.Server.Utils.Shim
import Snap.Server.Utils.SpamDetect


data InstantiatedModule = InstantiatedModule
  { imHandler :: Method -> [ByteString] -> Maybe (Snap ())
  , imStatics :: Map [ByteString] (FilePath, MimeType) }

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
              , imStatics = Map.fromList [(mount, (path, mime))
                                         | StaticFile path mount mime <- statics] }

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
handleStaticFiles ms components =
  case mapMaybe (\m -> Map.lookup components (imStatics m)) ms of
    [(path, mime)] -> Just (staticFile mime path)
    [] -> Nothing
    _ -> error $ "Multiple handlers for the same static file: " ++ show components

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

config :: Int -> Config Snap a
config port =
  let stderrlogger = ConfigIoLog (Char8.hPutStrLn stderr)
  in setAccessLog stderrlogger
     . setErrorLog stderrlogger
     . setPort port
     $ defaultConfig

actionPenalty :: SpamAction -> Double
actionPenalty PlayRunStart = 0.2
-- curved increase from 0 at tm=0 to 2 at tm=1
actionPenalty (PlayRunTimeoutFraction tm) = (1 - 1 / (tm + 1)) * (2 / (1 - (1 / (1 + 1))))
actionPenalty PlaySave = 1.4

spamConfig :: SpamConfig SpamAction
spamConfig = SpamConfig
  { spamActionPenalty = actionPenalty
  , spamHalfTimeSecs = 10
  , spamThreshold = 3.0
  , spamForgetBelowScore = 0.1
  , spamForgetIntervalSecs = 3600 }

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  options <- Opt.parseOptions $ Opt.Interface defaultOptions $ Map.fromList
    [("--proxied", Opt.Flag "Assumes the server is running behind a proxy that sets \
                            \X-Forwarded-For, instead of using the source IP of a \
                            \request for rate limiting."
                            (\o -> o { oProxied = True }))
    ,("--dbdir", Opt.Setter "Sets directory to store pastes.db in."
                            (\o s -> o { oDBDir = s }))
    ,("--secretkey", Opt.Setter
        "Required. Path to file that contains the secret key \
        \of this server (for communication with playground workers). \
        \The file should contain 32 random bytes \
        \in hexadecimal notation."
        (\o s -> o { oSecKeyFile = s }))
    ,("--adminpassfile", Opt.Setter
        "Path to file that contains the admin password. If not given, \
        \the admin interface is disabled. CR/LF is stripped from the start \
        \and end of the file content."
        (\o s -> o { oAdminPassFile = Just s }))
    ,("--preloadworkers", Opt.Setter
        "Path to file that contains, on each line, a hostname and a public \
        \key (in hexadecimal), separated by a space. Same format as what is \
        \accepted in the admin interface. These workers will be added on \
        \startup of the server, to aid automatic installations."
        (\o s -> o { oPreloadFile = Just s }))
    ,("--port", Opt.Setter
        "Port to listen on for http connections."
        (\o s -> o { oPort = case readMaybe s of Just n -> n ; Nothing -> error "Invalid --port value" }))
    ,("--help", Opt.Help)
    ,("-h", Opt.Help)]

  spam <- initSpamDetect spamConfig
  pagesvar <- pagesFromDisk >>= newTVarIO

  when (oSecKeyFile options == "") $ die "'--secretkey' is required"
  serverseckey <- do
    mskey <- (hexDecodeBS . trim >=> Sign.readSecretKey) <$> readFile (oSecKeyFile options)
    case mskey of
      Just skey -> return skey
      Nothing -> die "Invalid secret key in file"

  adminPassword <- case oAdminPassFile options of
    Nothing -> return Nothing
    Just fname -> do
      contents <- BS.readFile fname
      let isCRLF c = c `elem` [10, 13]
      return (Just (BS.dropWhile isCRLF (BS.dropWhileEnd isCRLF contents)))

  preloadWorkers <- case oPreloadFile options of
    Nothing -> return []
    Just fname -> do
      lns <- filter (not . null) . lines <$> readFile fname
      sequence [case break (==' ') line of
                  (host, ' ' : pubkey)
                    | Just pubkey' <- hexDecodeBS pubkey >>= Sign.readPublicKey ->
                        return (Char8.pack host, pubkey')
                    | otherwise -> die $ "Invalid pubkey in --preloadworkers file: " ++ pubkey
                  _ -> die $ "Invalid line in --preloadworkers file: " ++ line
               | line <- lns]

  let Sign.PublicKey serverpubkey = Sign.publicKey serverseckey
  putStrLn $ "My public key: " ++ hexEncode serverpubkey

  _ <- Signal.installHandler Signal.sigUSR1 (Signal.Catch (refreshPages pagesvar)) Nothing

  withDatabase (oDBDir options) $ \db -> do
    let gctx = GlobalContext
                 { gcSpam = spam
                 , gcDb = db
                 , gcPagesVar = pagesvar
                 , gcServerSecretKey = serverseckey
                 , gcAdminPassword = adminPassword
                 , gcPreloadWorkers = preloadWorkers }

    modules <- sequence [playModule]
    instantiates gctx options modules $ \modules' -> do
      httpServe (config (oPort options)) (server options modules')
