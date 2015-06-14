
module BenchmarkHistory where

import           Data.Time
import           Control.DeepSeq
import           GHC.Stats
import           Data.Csv
import           Data.Int(Int64)
import           GHC.Generics
import           System.Directory (doesFileExist)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import           Control.Arrow (second)
import           GHC.Conc (pseq)
import qualified Data.List as L



newtype TimeStamp = TimeStamp { getTimeStamp :: LocalTime }
  deriving (Read,Show,Generic)

instance NFData TimeStamp

instance FromField TimeStamp where
  parseField = fmap (TimeStamp . read) . parseField

instance ToField TimeStamp where
  toField = toField . show . getTimeStamp



data Stats = Stats
  { timeStamp   :: !TimeStamp
  , runningTime :: !Double
  , bytesAlloc  :: !Int64
  }
  deriving (Read,Show,Generic)

instance DefaultOrdered   Stats
instance FromNamedRecord  Stats
instance ToNamedRecord    Stats
instance FromRecord       Stats
instance ToRecord         Stats
instance NFData           Stats



gcStatDiff :: Num a => (GCStats -> a) -> GCStats -> GCStats -> a
gcStatDiff f pre post = f post - f pre
{-# Inline gcStatDiff #-}

-- | Go from two @GCStats@ to a difference in terms of @Stats@.

difference :: Int -> LocalTime -> GCStats -> GCStats -> Stats
difference mul time pre post = Stats
                                { timeStamp   = TimeStamp time
                                , runningTime = gcStatDiff cpuSeconds     pre post / fromIntegral mul
                                , bytesAlloc  = gcStatDiff bytesAllocated pre post `div` fromIntegral mul
                                }

-- | Benchmark a function. The function should take a /considerable amount
-- of time/ to finish, since the benchmarking system is designed to measure
-- coarse-grained timings.

benchmark :: NFData b => Int -> String -> (a -> b) -> a -> IO ()
benchmark mul' file fun x = do
  let mul = max 1 mul'
  dfe <- doesFileExist file
  (h,xs) <- if dfe
              then do BSL.readFile file >>= (return . either error (second V.toList) . decodeByName)
              else do return (V.empty,[]) :: IO (Header, [Stats])
  time <- fmap zonedTimeToLocalTime getZonedTime
  pre <- getGCStats
  res <- V.foldM' (\a b -> b >> return ()) () $ V.map (call fun) $ V.replicate mul x
  post <- pseq res $ getGCStats
  let ys = difference mul time pre post : xs
  putStrLn ""
  print file
  mapM_ print . reverse . take 10 $!! ys
  BSL.writeFile file $ encodeDefaultOrderedByName ys
{-# NoInline benchmark #-}

call :: NFData b => (a -> b) -> a -> IO b
call f x = return $!! f x
{-# NoInline call #-}

