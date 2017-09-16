module Utils
    ( getCurrentTime
    , sample
    ) where

import qualified Data.Time.Clock.POSIX as Time
import qualified System.Random         as Rnd


getCurrentTime :: IO Int
getCurrentTime =
    truncate . (* 1000000) <$> Time.getPOSIXTime


sample :: Int -> [a] -> IO [a]
sample n list = do
    randomGen <- Rnd.newStdGen
    let indices = take n $ Rnd.randomRs (0, length list - 1) randomGen
    return $ map (list !!) indices
