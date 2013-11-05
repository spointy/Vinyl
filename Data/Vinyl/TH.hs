{-# LANGUAGE TypeOperators, DataKinds, TemplateHaskell #-}
module Data.Vinyl.TH
  ( makeVinylFields
  , makeVinylRecord
  , makeVinylExtendedRecord
  ) where

import Control.Applicative
import Data.Vinyl
import Language.Haskell.TH

-- | Take a list of (field name, field type) and generate field definition
--   boilerplate from it.
makeVinylFields :: [(String, Q Type)] -> Q [Dec]
makeVinylFields fields = mapM (uncurry makeVinylField) fields

makeVinylField :: String -> Q Type -> Q Dec
makeVinylField name typ = valD (varP (mkName name)) body []
  where
    body = normalB [| Field :: $(litT (strTyLit name)) ::: $typ |]

-- | Create an alias for a list of fields.
makeVinylRecord :: String -> [Name] -> Q [Dec]
makeVinylRecord name fields = do
  mkVinylRecord name promotedNilT fields

-- | Create an alias that extends an existing list of fields with a given list.
makeVinylExtendedRecord :: String -> Name -> [Name] -> Q [Dec]
makeVinylExtendedRecord name basename fields = do
  basetype <- getVinylRecordType basename
  mkVinylRecord name (getVinylRecordType basename) fields

mkVinylRecord :: String -> Q Type -> [Name] -> Q [Dec]
mkVinylRecord name basetype fields = do
  let types = map getVinylFieldType fields
  fieldTypeList <- foldr (\ ty tys -> [t| $ty ': $tys |]) basetype types
  return $ [ TySynD (mkName name) [] fieldTypeList ]

getVinylRecordType :: Name -> Q Type
getVinylRecordType name = do
  info <- reify name
  case info of
    TyConI (TySynD _ [] typ) -> return typ
    _ -> fail $ "getVinylRecordType: the Name " ++ show name
                  ++ " does not point to a potential record type."

getVinylFieldType :: Name -> Q Type
getVinylFieldType name = do
  info <- reify name
  case info of
    VarI _ typ _ _ -> return typ
    _ -> fail $ "getVinylFieldType: the Name " ++ show name
                  ++ " does not point to a value with a type."
