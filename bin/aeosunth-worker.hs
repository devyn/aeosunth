import Network.IRC.Aeosunth.Config
import Network.IRC.Aeosunth.Logger
import Network.IRC.Aeosunth.Worker
          
main = do args <- getArgs
          conf <- configFromFile (if null args then "aeosunth-worker.yaml" else head args) workerDefaults
          startWorker conf
