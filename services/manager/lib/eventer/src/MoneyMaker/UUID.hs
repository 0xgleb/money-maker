{-# LANGUAGE TemplateHaskell #-}

module MoneyMaker.UUID where

import Prelude

import Data.Maybe (fromMaybe)

import qualified Data.UUID                 as UUID
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH.Q

uuid :: TH.Q.QuasiQuoter
uuid = TH.Q.QuasiQuoter{..}
  where
    quoteExp s = do
      loc <- TH.location

      let compileTimeError
            = show s ++ " is not a UUID"

          runTimeError = concat
            [ TH.pprint loc , ": " , show s
            , " was a UUID at compile-time, but is not at runtime"
            ]

      case UUID.fromString s of
        Nothing -> fail compileTimeError
        Just _  -> [| fromMaybe (error runTimeError) (UUID.fromString s) |]

    quotePat _  = fail "Can't use a UUID as a pattern"
    quoteType _ = fail "Can't use a UUID as a type"
    quoteDec _  = fail "Can't use a UUID as a declaration"
