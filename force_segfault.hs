import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, liftM2, void, when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)

import Sound.OSC

infixl 1 >>=||
(>>=||) :: (Traversable t, Monad m) => m a -> t (a -> m b) -> m (t b)
(>>=||) m = mapM (m >>=)

void_ :: IO [()] -> IO ()
void_ = void

send :: IO UDP -> [Message] -> IO ()
send udpO ms = mapM_ (withTransport udpO . sendMessage) ms

runServer :: Int -> Int -> String -> IO ()
runServer pI pO aO = send (openUDP aO pO) messages

messages :: [Message]
messages =
  [ Message "/ch/01/mix/fader" [Float 0]
  , Message "/ch/02/mix/fader" [Float 0]
  ] ++ messages

main :: IO ()
main = runServer 10024 9010 "127.0.0.1"

