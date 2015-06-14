
module BenchmarkHistory where

import           Data.Time
import           Control.DeepSeq
import           GHC.Stats
import           Data.Csv
import           Data.Int(Int64)
import           GHC.Generics
import           System.Mem
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
                                , bytesAlloc  = gcStatDiff cumulativeBytesUsed pre post `div` fromIntegral mul
                                }

-- | Benchmark a function. The function should take a /considerable amount
-- of time/ to finish, since the benchmarking system is designed to measure
-- coarse-grained timings.

benchmark
  :: (NFData e, NFData a, NFData b)
  => Int            -- | multiplicity of the benchmark run
  -> String         -- | name of the benchmark file
  -> (a -> e)       -- | environment generator (not benched)
  -> (e -> a -> b)  -- | given environment, input, create output
  -> a              -- | input
  -> IO ()          -- | run everything, don't communicate back
benchmark mul' file env fun x = do
  let mul = max 1 mul'
  dfe <- doesFileExist file
  (h,xs) <- if dfe
              then do BSL.readFile file >>= (return . either error (second V.toList) . decodeByName)
              else do return (V.empty,[]) :: IO (Header, [Stats])
  time <- fmap zonedTimeToLocalTime getZonedTime
  performGC
  preE <- getGCStats
  let !e = env x
  deepseq e $ performGC
  pre <- getGCStats
  print $ cumulativeBytesUsed preE
  print $ cumulativeBytesUsed pre
  res <- V.foldM' (\a b -> b >> return ()) () $ V.map (call $ fun e) $ V.replicate mul x
  post <- pseq res $ getGCStats
  print $ cumulativeBytesUsed post
  let ys = difference mul time pre post : xs
  putStrLn ""
  print file
  mapM_ print . reverse . take 10 $!! ys
  let (gt,gm) = good 10 ys
  print (gt,gm)
  if gt <! 1.05 && gm <! 1.05 then print "OK" else print "performance regression!"
  BSL.writeFile file $ encodeDefaultOrderedByName ys
{-# NoInline benchmark #-}

infixl 7 <!

-- | @a <! b@ is like @a < b@ but will still return @True@ if @a@ or @b@ is
-- @NaN@

a <! b
  | isNaN a   = True
  | isNaN b   = True
  | otherwise = a < b
{-# Inline (<!) #-}

call :: NFData b => (a -> b) -> a -> IO b
call f x = return $!! f x
{-# NoInline call #-}

good :: Int -> [Stats] -> (Double,Double)
good _ []  = (1,1)
good _ [x] = (1,1)
good l (x:ys) = let zs = take l ys
                    k  = L.genericLength zs
                in  ( k * runningTime x / (sum $ map runningTime zs) , k * fromIntegral (bytesAlloc x) / fromIntegral (sum $ map bytesAlloc zs))

