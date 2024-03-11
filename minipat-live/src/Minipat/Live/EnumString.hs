{-# LANGUAGE TemplateHaskellQuotes #-}

module Minipat.Live.EnumString
  ( EnumString (..)
  , allEnumStrings
  , deriveEnumString
  )
where

import Data.Char (toUpper)
import Data.Proxy (Proxy (..))
import Language.Haskell.TH
  ( Body (..)
  , Clause (..)
  , Con (..)
  , Dec (..)
  , DerivClause (..)
  , DerivStrategy (..)
  , Exp (..)
  , Lit (..)
  , Match (..)
  , Name
  , Pat (..)
  , Q
  , Quote (..)
  , Type (..)
  , appT
  , conT
  , cxt
  , funD
  , instanceD
  , lookupValueName
  , mkName
  )

class (Enum a) => EnumString a where
  toEnumString :: String -> Maybe a
  fromEnumString :: a -> String

allEnumStrings :: forall a. (Bounded a, EnumString a) => Proxy a -> [String]
allEnumStrings _ = fmap (fromEnumString @a) [minBound .. maxBound]

mkFromEnumStringMatch :: Name -> String -> Match
mkFromEnumStringMatch n s = Match (ConP n [] []) (NormalB (LitE (StringL s))) []

mkToEnumStringMatch :: Name -> Name -> String -> Match
mkToEnumStringMatch j n s = Match (LitP (StringL s)) (NormalB (AppE (ConE j) (ConE n))) []

realDeriveData :: Name -> [Name] -> Dec
realDeriveData tyName conNames =
  let cons = fmap (`NormalC` []) conNames
      stockDerivs = DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Enum, ConT ''Bounded]
  in  DataD [] tyName [] Nothing cons [stockDerivs]

realDeriveEnumString :: Name -> [(Name, String)] -> Q Dec
realDeriveEnumString name pairs =
  instanceD
    (cxt [])
    (appT (conT ''EnumString) (conT name))
    [ funD (mkName "fromEnumString") $ pure $ do
        arg <- newName "x"
        let matches = fmap (uncurry mkFromEnumStringMatch) pairs
            body = LamE [VarP arg] (CaseE (VarE arg) matches)
        pure (Clause [] (NormalB body) [])
    , funD (mkName "toEnumString") $ pure $ do
        arg <- newName "y"
        Just justName <- lookupValueName "Just"
        Just nothingName <- lookupValueName "Nothing"
        let matches = fmap (uncurry (mkToEnumStringMatch justName)) pairs
            defMatch = Match WildP (NormalB (ConE nothingName)) []
            body = LamE [VarP arg] (CaseE (VarE arg) (matches ++ [defMatch]))
        pure (Clause [] (NormalB body) [])
    ]

deriveEnumString :: String -> [String] -> Q [Dec]
deriveEnumString strTyName strNames = do
  tyName <- newName strTyName
  let cap = \case "" -> ""; c : cs -> toUpper c : cs
  names <- traverse (\s -> newName (strTyName ++ cap s)) strNames
  let namePairs = zip names strNames
      derivedData = realDeriveData tyName names
  derivedEnumString <- realDeriveEnumString tyName namePairs
  pure [derivedData, derivedEnumString]
