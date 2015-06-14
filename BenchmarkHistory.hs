
module BenchmarkHistory where

import           Control.Arrow (second)
import           Control.DeepSeq
import           Data.Csv
import           Data.Function (fix)
import           Data.Int(Int64)
import           Data.Time
import           GHC.Conc (pseq)
import           GHC.Generics
import           GHC.Stats
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.Vector as V
import           Statistics.Sample
import           System.Directory (doesFileExist)
import           System.Exit
import           System.Mem
import           Text.Printf



newtype TimeStamp = TimeStamp { getTimeStamp :: LocalTime }
  deriving (Read,Show,Generic)

instance NFData TimeStamp

instance FromField TimeStamp where
  parseField = fmap (TimeStamp . read) . parseField

instance ToField TimeStamp where
  toField = toField . show . getTimeStamp



newtype GCStatistics = GCStatistics { getGCStatistics :: GCStats }
  deriving (Read,Show,Generic)

instance NFData GCStatistics where
  rnf (GCStatistics !x) = ()

instance FromField GCStatistics where
  parseField = fmap (GCStatistics . read) . parseField

instance ToField GCStatistics where
  toField = toField . show . getGCStatistics



data Stats = Stats
  { timeStamp   :: !TimeStamp
  , preStats    :: !GCStatistics
  , postStats   :: !GCStatistics
  , multiplier  :: !Int
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
  -> IO ExitCode    -- | run everything, return exit code based on performance
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
  res <- V.foldM' (\a b -> b >> return ()) () $ V.map (call $ fun e) $ V.replicate mul x
  post <- pseq res $ getGCStats
  putStrLn ""
  putStrLn file
  let ys = Stats (TimeStamp time) (GCStatistics pre) (GCStatistics post) mul : xs
  exit <- basicStats $ eachBlock cpuSeconds (\m o n -> (n-o) / fromIntegral m) $ toBlocks [1,1 ..] ys
  BSL.writeFile file $ encodeDefaultOrderedByName ys
  return exit
{-# NoInline benchmark #-}

call :: NFData b => (a -> b) -> a -> IO b
call f x = return $!! f x
{-# NoInline call #-}

-- | Divide data into blocks

toBlocks :: [Int] -> [a] -> [[a]]
toBlocks _      [] = []
toBlocks []     _  = error "not enough block division information"
toBlocks (b:bs) xs = let (ys,zs) = splitAt b xs
                     in  ys : toBlocks bs zs

defbs = replicate 4 1 ++ fix (map (*2) . (1:))

-- | for each block, perform an op and return first time stamp and op
-- result

eachBlock
  :: (GCStats -> r)
  -> (Int -> r -> r -> Double)
  -> [[Stats]]
  -> [(TimeStamp, Double)]
eachBlock f cmb = map go . filter (not . null)
  where go xs = (timeStamp $ head xs, mean $ V.map oneStat $ V.fromList xs)
        oneStat s = cmb (multiplier s) (f . getGCStatistics $ preStats s) (f . getGCStatistics $ postStats s)

-- | Statistics for data

basicStats :: [(TimeStamp, Double)] -> IO ExitCode
basicStats [] = return ExitSuccess
basicStats xs' = do
  let xs = V.fromList xs'
  let x  = V.head xs
  let μ = mean   $ V.map snd xs
  let σ = stdDev $ V.map snd xs
  printf "μ %f σ %f current: %f   (%s)\n" μ σ (snd x) (show $ snd x <= μ + σ)
  return $ if snd x <= μ + σ
             then ExitSuccess
             else ExitFailure 1

