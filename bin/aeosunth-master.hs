import Network.IRC.Aeosunth.Config
import Network.IRC.Aeosunth.Master

main = do args <- getArgs
          conf <- configFromFile (if null args then "aeosunth-master.yaml" else head args) masterDefaults
          startMaster conf
