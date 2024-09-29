{-# LANGUAGE TemplateHaskell #-}

module DeriveJsonNoTypePrefixElmBridge
  ( deriveJsonNoTypeNamePrefix',
    deriveToJsonNoTypeNamePrefix',
    deriveElmDef',
  )
where

import qualified Data.Aeson as Json
import Data.Aeson.TH (deriveToJSON)
import Data.Aeson.Types
  ( Options (fieldLabelModifier),
  )
import Data.Char
  ( toLower,
  )
import Elm.Derive
import Language.Haskell.TH
  ( Dec,
    Name,
    Q,
    nameBase,
  )

-- fork from <https://gitlab.com/igrep/deriveJsonNoPrefix>

deriveJsonNoTypeNamePrefix' :: Name -> Q [Dec]
deriveJsonNoTypeNamePrefix' name =
  deriveBoth Json.defaultOptions {fieldLabelModifier = dropPrefix name} name

deriveTo :: Json.Options -> Name -> Q [Dec]
deriveTo o n = (++) <$> deriveElmDef o n <*> deriveToJSON o n

-- deriveToJsonのみ自動生成する（deviveFromJSONは手動で作成する場合に使用する）
deriveToJsonNoTypeNamePrefix' :: Name -> Q [Dec]
deriveToJsonNoTypeNamePrefix' name =
  deriveTo Json.defaultOptions {fieldLabelModifier = dropPrefix name} name

deriveElmDef' :: Name -> Q [Dec]
deriveElmDef' name =
  deriveElmDef Json.defaultOptions {fieldLabelModifier = dropPrefix name} name

dropPrefix :: Name -> String -> String
dropPrefix name = firstLower . drop (length $ nameBase name)

firstLower :: String -> String
firstLower (x : xs) = toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"
