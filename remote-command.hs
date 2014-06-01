import System.Environment
import System.Exit
import System.Process
import Control.Applicative
import Control.Concurrent.Async
import Data.Maybe
import Data.Traversable (Traversable)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

type MachineName = ByteString

-- the Traversable may not be a good choice here, since we cannot so
-- any form of filter, which is probably what we want.

filterReachable :: Traversable c => c MachineName -> IO (c (Maybe MachineName))
filterReachable = mapConcurrently isOK
  where isOK m = checkOK <$> mkProcessForNode "uptime" noInput m
        checkOK (RemoteCommandResult m ExitSuccess _ _) = Just m
        checkOK _ = Nothing

data RemoteCommandResult = RemoteCommandResult
  { rcr_machine :: MachineName
  , rcr_result  :: ExitCode
  , rcr_stdout  :: ByteString
  , rcr_stderr  :: ByteString
  } deriving (Eq, Ord, Show)

mkProcessForNode :: ByteString -> RemoteCommandInput -> MachineName -> IO RemoteCommandResult
mkProcessForNode command input node = (\(c, o, e) -> RemoteCommandResult node c (BC.pack o) (BC.pack e)) <$> readProcessWithExitCode
 "ssh" [ "dollar-gate"
       , unwords $ [ "ssh"
                   , "-o ConnectTimeout=5"
                   , "-o StrictHostKeyChecking=no"
                   , "-q"
                   , "-l", "root"
                   , BC.unpack node
                   , "'" ++ BC.unpack command ++ "'"
                   ]
       ] (BC.unpack . input $ node)

type RemoteCommandInput = MachineName -> ByteString

noInput :: RemoteCommandInput
noInput = const BC.empty

mainTest command nodes = do
  result <- mapConcurrently (mkProcessForNode command noInput) nodes
  return result

mainTestWithInput command input nodes = do
  result <- mapConcurrently (mkProcessForNode command input) nodes
  return result

mainTestWithScript :: Traversable c => RemoteCommandInput -> c MachineName -> IO (c RemoteCommandResult)
mainTestWithScript = mainTestWithInput "/bin/bash"

script :: RemoteCommandInput
script _ = BC.unlines
  [ "cat /proc/meminfo | head -n 1"
  , "cat /proc/cpuinfo | grep -c '^processor'"
  , "dmidecode -s system-product-name"
  ]
