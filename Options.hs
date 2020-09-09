{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Options (
    Interface(..), OptValue(..), parseOptions
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (findIndices, intercalate, partition)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (Handle, hPutStr, hPutStrLn, stdout, stderr)


data Interface a = Interface a (Map String (OptValue a))

data OptValue a = Flag String (a -> a)
                | Setter String (a -> String -> a)
                | Help

data ParseResult a = PRHelp | PRDone a

parseOptions :: forall a. Interface a -> IO a
parseOptions (Interface initstate mp) =
    fmap (go initstate) getArgs >>= \case
        Right PRHelp -> showHelpText stdout >> exitSuccess
        Right (PRDone res) -> return res
        Left err -> hPutStrLn stderr err >> showHelpText stderr >> exitFailure
  where
    go :: a -> [String] -> Either String (ParseResult a)
    go state [] = Right (PRDone state)
    go state (opt@('-':_) : rest) =
        case Map.lookup opt mp of
            Just (Flag _ f) -> go (f state) rest
            Just (Setter _ f) -> case rest of
                [] -> Left $ "Expected argument to option '" ++ opt ++ "'"
                ('-':_) : _ -> Left $ "Expected argument after option '" ++ opt ++ "', not another option"
                arg : rest' -> go (f state arg) rest'
            Just Help -> Right PRHelp
            Nothing -> Left $ "Unrecognised option '" ++ opt ++ "'"
    go _ (arg : _) = Left $ "Unexpected bare argument '" ++ arg ++ "'"

    showHelpText :: Handle -> IO ()
    showHelpText handle = helpText (Map.assocs mp) >>= hPutStr handle

helpText :: [(String, OptValue a)] -> IO String
helpText optList = do
    progname <- getProgName
    let (helpOpts, nonHelpOpts) = partition (isHelp . snd) optList
        formatted = map formatOption nonHelpOpts
        longest = maximum (map (length . fst) formatted)
        optprefix = "  "
        optsep = "  "
        indent = optprefix ++ replicate longest ' ' ++ optsep
        wrapwid = max 30 (80 - length indent)
    return $ unlines $
        ["Usage:", "  " ++ progname ++ " [options...]", "Options:"]
        ++ concat [(optprefix ++ padRight longest lhs ++ optsep ++ head rhs')
                        : map (indent ++) (tail rhs')
                  | (lhs, rhs) <- formatted
                  , let rhs' = wrapText wrapwid rhs]
        ++ if null helpOpts then []
           else [optprefix ++ padRight longest (intercalate "/" (map fst helpOpts)) ++ optsep
                    ++ "Show this help"]

isHelp :: OptValue a -> Bool
isHelp Help = True
isHelp _ = False

formatOption :: (String, OptValue a) -> (String, String)
formatOption (opt, Flag d _) = (opt, d)
formatOption (opt, Setter d _) = (opt ++ " STR", d)
formatOption (opt, Help) = (opt, "Show this help")

padRight :: Int -> String -> String
padRight w s = s ++ replicate (max 0 (w - length s)) ' '

wrapText :: Int -> String -> [String]
wrapText wid source
  | length source <= wid = [source]
  | otherwise =
      case takeWhile (<= wid) (findIndices (== ' ') source) of
          [] -> [source]
          spaces -> let idx = last spaces
                        (pre, _ : post) = splitAt idx source
                    in pre : wrapText wid post
