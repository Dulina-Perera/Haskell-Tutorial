import qualified Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Data.Text.Array (run)
import System.Process
import Text.Printf

outFilePath :: FilePath
outFilePath = "wave.bin"

volume :: Float
volume = 0.5

sampleCnt :: Float
sampleCnt = 48000

wave :: [Float]
wave = map (((* volume) . sin) . (* step)) [0..sampleCnt]
    where step = 0.05

save :: FilePath -> [Float] -> IO ()
save path xs = BL.writeFile path $ BB.toLazyByteString $ foldMap BB.floatLE xs

play :: Float -> IO ProcessHandle
play audioRate = do
    save outFilePath wave

    let duration = sampleCnt / audioRate :: Float
    runCommand $ printf "ffplay -f f32le -ar %.3f %s -nodisp -autoexit -t %.3f" audioRate outFilePath duration

main :: IO ()
main = do
    let audioRate = 48000 :: Float
    processHandle <- play audioRate
    _ <- waitForProcess processHandle

    putStrLn "Audio playback completed."
