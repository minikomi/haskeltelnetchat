module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Network
import System.IO
import System.IO.Error
import Control.Monad

port :: PortID
port = PortNumber 1234

type Name = String

data User = User { name   :: Name
                 , handle :: Handle
                 }

data ChatEvent = Message Name String
               | Quit Name
               | Join User

data Hub = Hub { users :: [User]
               , chan :: Chan ChatEvent
           }

writeUser :: User -> String -> IO ()
writeUser (User _ h) msg = hPutStr h (msg ++ "\n")

broadcastLoop :: Hub -> IO ()
broadcastLoop h@(Hub us ch) = do
  e <- readChan ch
  handler e h
  where
    handler ev hub = case ev of
                    (Message n s) -> do mapM_ (\ u@(User n' _) -> when (n /= n') $ writeUser u (n ++ ": " ++ s)) us
                                        broadcastLoop hub
                    (Join u@(User n _)) -> let newusers  = u : us
                                               usercount = (show $ length newusers) in
                                             do print $ n ++ " joined. " ++ " Currently Chatting: " ++ usercount
                                                mapM_ (\u' -> writeUser u' (n ++ " joined.")) us
                                                writeUser u ("Welcome "  ++ n ++". Currenly " ++ usercount )
                                                broadcastLoop (Hub newusers ch)
                    (Quit n) -> let newusers = filter (\ (User n' _) -> n /= n') us
                                    usercount = (show $ length newusers) in
                                    do print $ n ++ " quit. " ++ " Currently chatting: " ++ usercount
                                       mapM_ (\ u -> writeUser u (n ++ " quit!")) newusers
                                       broadcastLoop (Hub newusers ch)


userLoop :: User -> Chan ChatEvent -> IO ()
userLoop user@(User n h) ch =
  do eitherLn <- try (hGetLine h)
     case eitherLn of
        Left e -> if isEOFError e
                  then do
                        writeChan ch $ (Quit n)
                        return ()
                  else ioError e
        Right ln -> do
          writeChan ch (Message n ln)
          userLoop user ch

acceptLoop :: Chan ChatEvent -> IO ()
acceptLoop evChan =
  bracket (listenOn port) sClose $ \s -> acceptLoop' s (0::Int)
  where acceptLoop' s n   = bracket (accept s) (\(h, _, _) -> hClose h) (handler s n)
        handler s n (h,_,_) =
          do hPutStr h "Please enter your name: "
             eitherLn <- try (hGetLine h)
             case eitherLn of
                  Left e -> if isEOFError e
                            then do return ()
                            else ioError e
                  Right ln -> do
                    let u = (User (filter (/= '\r') ln) h)
                    writeChan evChan (Join u)
                    forkIO $ userLoop u evChan
                    acceptLoop' s (succ n)

main :: IO ()
main = do
  evChan <- newChan
  let h = Hub [] evChan
  forkIO $ broadcastLoop h
  acceptLoop evChan
