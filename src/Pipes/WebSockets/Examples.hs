{-| Examples of usage. In this module we implement a few standard ways
    one may wish to use this library.
-}
module Pipes.WebSockets.Examples (
    echoStdInOut
  )
  where

import           Data.Text
import           Debug.Trace                    (traceM)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                  as P
import           Pipes.WebSockets
import qualified Network.WebSockets             as WS
import           Control.Monad.Trans.Reader     (ReaderT (runReaderT))

-- | Uses [Pipes.Concurrent](https://hackage.haskell.org/package/pipes-concurrency) 
--   to start up threads for reading from and writing to a single 
--   [WebSocket](https://hackage.haskell.org/package/websockets) connection.
--   Input to send is read from `stdin` and response are written to `stdout`.
--
--   Try it out on [echo.websocket.org](http://www.websocket.org) with:
--
--   > -- wsTest.hs
--   > module Main where 
--   >
--   > import Pipes.WebSockets.Examples
--   >
--   > main :: IO ()
--   > main = echoStdInOut "echo.websocket.org" 80 "/"
echoStdInOut :: String  -- ^ Server to connect to
             -> Int     -- ^ Port
             -> String  -- ^ Path from the server, i.e. "/".
             -> IO ()
echoStdInOut host port path = do
    -- Setup a "mailbox" for getting messages from and placing them in.
    (output, input) <- spawn unbounded
    
                 -- Take messages from the socket and put them in the output.
    let msgsFrom = runEffect $ wsIn >-> toOutput output
                 -- Read from stdin and send msgs to the server.
        msgsTo   = runEffect $ for P.stdinLn
                                   (\s -> yield (pack s) >-> wsOut)
        --
        --  Read messages from the socket (which have been placed on the
        --  mailbox).
        msgsOut  = runEffect $ for (fromInput input) 
                                   (\s -> lift $ putStrLn ("Received: " ++ unpack s))

    lvf $ msgsOut

    WS.runClient host port path (\c -> do
                                lvf $ runReaderT msgsFrom c
                                runReaderT msgsTo c
                                )
    where
        lvf = liftIO . void . forkIO


