module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad.Free

import Concur.Core
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.ShiftMap

view :: v -> Widget v ()
view v = Widget $ liftF $ StepView v ()

receive :: TChan a -> Widget v a
receive ch = liftSafeBlockingIO $ atomically $ readTChan ch

to :: TChan v -> Widget v a -> Widget () a
to ch w = undefined

gh :: (a, b) -> TChan a -> TChan b -> TChan Int -> Widget () ()
gh (a, b) ach bch out = do
  r <- fmap Left (receive ach) <|> fmap Right (receive bch)
  liftSafeBlockingIO $ atomically $ writeTChan out 5
  case r of
    Left a'  -> gh (a', b) ach bch out
    Right b' -> gh (a, b') ach bch out

someFunc :: IO ()
someFunc = putStrLn "someFunc"
