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

listen :: IO UDP -> IO UDP -> IO ()
listen udpI udpO = (fromJust <$> withTransport udpI recvMessage) >>= (\m -> when (messageAddress m /= "/xremote") (void_ $ return m >>=|| [withTransport udpO . sendMessage, print]))
--listen udpI udpO = void_ $ withTransport udpI recvBundle >>=|| [withTransport udpO . sendBundle, print]

runServer :: Int -> Int -> String -> IO ()
runServer pI pO aO = forever $ listen (udp_server pI) (openUDP aO pO)

main :: IO ()
main = runServer 10024 9010 "127.0.0.1"

