module Config where

import           Data.Yaml    (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           GPIO.Types   (Pin)

data Config = Config
  { rows    :: [Pin]
  , columns :: [Pin]
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

