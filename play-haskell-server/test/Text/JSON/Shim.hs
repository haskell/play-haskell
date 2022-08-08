module Text.JSON.Shim (
  J.JSValue(..),
  encodeJSON,
  decodeJSON,
  jsObject,
  jsString,
  J.fromJSObject,
  J.fromJSString,
) where

import qualified Text.JSON as J
import qualified Text.JSON.String as J


encodeJSON :: J.JSValue -> String
encodeJSON = flip J.showJSValue ""

decodeJSON :: String -> Maybe J.JSValue
decodeJSON s = case J.runGetJSON J.readJSValue s of
                 Left _ -> Nothing
                 Right res -> Just res

jsObject :: [(String, J.JSValue)] -> J.JSValue
jsObject = J.JSObject . J.toJSObject

jsString :: String -> J.JSValue
jsString = J.JSString . J.toJSString
