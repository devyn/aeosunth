import qualified Network.IRC.Aeosunth.Config as Config
import           Network.IRC.Aeosunth.Worker
import           System.Exit
import           System.Log (criticalM)

main = do args    <- getArgs
          tryConf <- Config.fromFile (if null args then "aeosunth-worker.yaml" else head args) workerDefaults
          case tryConf of
               Right conf -> startWorker conf
               Left  err  -> do criticalM "aeosunth.config" $ "Exiting due to configuration parsing failure: " ++ err
                                exit 1
