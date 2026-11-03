module Config where

import           Data.List           (nub)
import           Data.Yaml           (FromJSON, ToJSON, decodeFileEither)
import qualified Data.Map.Strict     as M
import           GHC.Generics (Generic)
import           GPIO.PinMap  (pinMap)
import           GPIO.Types   (Pin)
import           System.Directory    (getHomeDirectory)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))

data Config = Config
  { chip    :: String
  , rows    :: [Pin]
  , columns :: [Pin]
  } deriving (Eq, Show, Generic)

instance FromJSON Config
instance ToJSON Config

keyboardConfigFile :: FilePath
keyboardConfigFile = "kvm" </> "keyboard.yaml"

configPath :: Maybe FilePath -> Maybe FilePath -> FilePath -> FilePath
configPath explicit xdg home = case explicit of
  Just path -> path
  Nothing -> case xdg of
    Just dir | not (null dir) -> dir </> keyboardConfigFile
    _                         -> home </> ".config" </> keyboardConfigFile

defaultConfigPath :: IO FilePath
defaultConfigPath = do
  xdg  <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  pure (configPath Nothing xdg home)

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  decoded <- decodeFileEither path
  pure $ case decoded of
    Left err  -> Left (show err)
    Right cfg -> case validateConfig cfg of
      Left err -> Left err
      Right () -> Right cfg

validateConfig :: Config -> Either String ()
validateConfig cfg
  | length cfg.rows /= 5 = Left "keyboard configuration must contain exactly 5 rows"
  | length cfg.columns /= 8 = Left "keyboard configuration must contain exactly 8 columns"
  | length (nub cfg.rows) /= length cfg.rows = Left "keyboard rows must not contain duplicates"
  | length (nub cfg.columns) /= length cfg.columns = Left "keyboard columns must not contain duplicates"
  | not (all (`M.member` pinMap) (cfg.rows <> cfg.columns)) = Left "keyboard configuration contains an unmapped GPIO pin"
  | otherwise = Right ()
