import Network.IRC.Aeosunth.Client
import Network.IRC.Aeosunth.Config
import Network.IRC.Aeosunth.Logger
          
main = do args <- getArgs
          conf <- configFromFile (if null args then "aeosunth-client.yaml" else head args) masterDefaults
          startClient conf
