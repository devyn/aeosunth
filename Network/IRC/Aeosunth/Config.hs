module Network.IRC.Aeosunth.Config (fromFile, merge, get, Config(..)) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as YAML

data Config = Config { configFile :: FilePath
                     , configData :: YAML.Value
                     }

fromFile :: FilePath -> Maybe YAML.Value -> IO (Either String Config)

fromFile path def = (((\ a -> maybe a (merge a . Config path) def) . Config path <$>) . YAML.decodeEither) <$> B.readFile path

merge :: Config -> Config -> Config

merge (Config destName destVal) (Config _ srcVal) = Config destName $ mer destVal srcVal
  where mer (YAML.Object x) (YAML.Object y)       = YAML.Object $ HashMap.unionWith mer x y
        mer x y                                   = x

get :: Config
    -> [Text]           -- ^ Walking path to config key.
    -> Maybe YAML.Value -- ^ If found, value of config key.

get c path    = foldl g (Just $ configData c) path
  where g x k = x >>= \x' -> case x' of
                                  YAML.Object o -> HashMap.lookup k o
                                  _             -> Nothing


