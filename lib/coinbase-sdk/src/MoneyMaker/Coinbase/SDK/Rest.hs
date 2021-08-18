module MoneyMaker.Coinbase.SDK.Rest
  ( API
  , Candle(..)
  , Granularity(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Model

import qualified Data.Time.Clock as Time
import           Servant.API     ((:>))
import qualified Servant.API     as Servant
import qualified Servant.Client  as Servant

type API
  = "products" :> Servant.Capture "product-id" ProductId :> "candles"
  :> Servant.QueryParams "start" Time.UTCTime
  :> Servant.QueryParams "end" Time.UTCTime
  :> Servant.QueryParams "granularity" Granularity
  :> Servant.Get '[Servant.JSON] [Candle]

-- TODO: add a custom JSON instance
data Candle
  = Candle
      { time  :: Time.UTCTime
      , low   :: Price
      , high  :: Price
      , open  :: Price
      , close :: Price
      -- , volume :: Volume
      }

data Granularity
  = OneMinute -- ^ 60
  | FiveMinutes -- ^ 300
  | FifteenMinutes -- ^ 900
  | OneHour -- ^ 3600
  | SixHours -- ^ 21600
  | OneDay -- ^ 86400

data ProductId
