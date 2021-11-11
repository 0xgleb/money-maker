{-# LANGUAGE TemplateHaskell #-}

module MoneyMaker.Based.UUID where

import Prelude
import Protolude (Symbol)

import Data.Maybe (fromMaybe)

import qualified Data.UUID                 as UUID
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH.Q

-- | Custom UUID wrapper for differentiating between different kind of ids
-- This allows you to to have, for example, @Id User@ and @Id House@ and
-- the compiler will make sure that you don't mess up and use one in place
-- of another. (tag :: k) part allows you to tag it not only with types but
-- with type-level values of any "kind", for example, type-level strings
newtype Id (tag :: Symbol)
  = Id { getId :: UUID.UUID }
  deriving newtype (Show, Eq)

uuidQuasiQuoter :: TH.Q.QuasiQuoter
uuidQuasiQuoter = TH.Q.QuasiQuoter{..}
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
