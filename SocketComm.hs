module SocketComm where
import Network
import GHC.IO.Handle
import System.Directory (removeFile,doesFileExist)
import System.IO
import qualified Control.Exception as E
import Control.Concurrent (threadDelay)

data Communicator = Communicator { port :: PortID,
                                   sock :: Socket } deriving (Eq,Show)

newCommunicator :: PortNumber -> PortNumber -> IO Communicator
newCommunicator reciever sender = do
  let port = PortNumber sender
  sock <- listenOn (PortNumber reciever)
  return $ Communicator port sock

sendTo :: Communicator -> String -> IO ()
sendTo comm str = do
  handle <- connectTillSuccessful comm
  hPutStrLn handle str
  hClose handle

getFrom :: Communicator -> IO String
getFrom comm = do
  (handle,host,port) <- accept $ sock comm
  handle <- waitForInput handle
  output <- hGetLine handle
  return output

-- getPortID :: PortNumber -> PortID
-- getPortID p = PortNumber p

-- initializeSocket :: PortNumber -> IO Socket
-- initializeSocket p = listenOn $ getPortID p

-- communicateS :: PortID -> String -> IO String
-- communicateS portID strAck = do
--   handle <- serverOpenTillSuccessful portID
--   hPutStrLn handle strAck
--   handle <- waitForInput handle
--   nextReq <- hGetLine handle
--   hClose handle
--   return nextReq

-- communicateC :: Socket -> String -> IO String
-- communicateC sock strReq = do
--   (handle,host,port) <- accept $ sock
--   handle <- waitForInput handle
--   strAck <- hGetLine handle
--   hPutStrLn handle strReq
--   return strAck

terminateCommunicator :: Communicator -> IO ()
terminateCommunicator comm = sClose $ sock comm

waitForInput :: Handle -> IO Handle
waitForInput handle = do
      inputAvailableOnHandle <- E.try (hReady handle) :: IO (Either E.IOException Bool)
      case inputAvailableOnHandle of
        Right True -> return handle
        _ -> (threadDelay 100000) >> waitForInput handle
                      
connectTillSuccessful :: Communicator -> IO Handle
connectTillSuccessful comm = do
  x <- E.try (connectTo "localhost" (port comm)) :: IO (Either E.IOException Handle)
  case x of
    Left _ -> threadDelay 100000 >> connectTillSuccessful comm
    Right handle -> return handle
