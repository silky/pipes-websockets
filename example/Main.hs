module Main where

import Pipes.WebSockets.Examples

main :: IO ()
main = echoStdInOut "echo.websocket.org" 80 "/"
