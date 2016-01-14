module Pipes.WebSockets where

import           Pipes
import qualified Network.WebSockets             as WS
import           Control.Monad.Trans.Reader     (ReaderT
                                                , ask)


-- TODO: Make some kind of 'runClientEffect' thing to wrap these things.


-- | Transformer so that we can pass a connection to the output
--   of these consumers/producers.
type WebSocketsT = ReaderT WS.Connection



-- | WebSockets Producer. Given some WebSocket connection, it will yield
--   a single message that comes through.
wsInOnce :: (MonadIO m, WS.WebSocketsData a) => Producer a (WebSocketsT m) ()
wsInOnce = do
    conn <- lift ask
    x    <- liftIO (WS.receiveData conn)
    yield x


-- | WebSockets Producer. Given some WebSocket connection, it will yield
--   the messages that come through.
wsIn :: (MonadIO m, WS.WebSocketsData a) => Producer a (WebSocketsT m) ()
wsIn = wsInOnce >> wsIn


-- | WebSockets Consumer. Given some connection, it will send messages that
--   are passed into it.
wsOut :: (MonadIO m, WS.WebSocketsData a) => Consumer a (WebSocketsT m) ()
wsOut = do
    conn <- lift ask
    m    <- await
    liftIO $ WS.sendTextData conn m
