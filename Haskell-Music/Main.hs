import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable

volume :: Float
volume = 0.5

wave :: [Float]
wave = map (* volume) $ map sin $ map (* step) [0..48000]
    where step = 0.01

save :: FilePath -> [Float] -> IO ()
save path xs = BL.writeFile path $ BB.toLazyByteString $ foldMap BB.floatLE xs

main :: IO ()
main = save "wave.bin" wave
