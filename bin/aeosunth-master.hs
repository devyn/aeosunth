import qualified Network.IRC.Aeosunth.Config as Config
import           Network.IRC.Aeosunth.Master
import           System.Exit
import           System.Log (criticalM)

main = do args    <- getArgs
          tryConf <- Config.fromFile (if null args then "aeosunth-master.yaml" else head args) masterDefaults
          case tryConf of
               Right conf -> startMaster conf
               Left  err  -> do criticalM "aeosunth.config" $ "Exiting due to configuration parsing failure: " ++ err
                                exit 1

